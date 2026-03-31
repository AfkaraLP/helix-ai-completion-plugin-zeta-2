;;; zeta-complete.scm — AI ghost-text completion using Zeta-2 via LM Studio
;;;
;;; Uses the Zeta-2 SPM (suffix-prefix-middle) prompt format to predict
;;; edits at the cursor. Sends requests to LM Studio at localhost:1234.
;;; Shows completions as inline ghost text (inlay hints), accepted with Tab.

(require "helix/editor.scm")
(require "helix/misc.scm")
(require (prefix-in helix.static. "helix/static.scm"))
(require (prefix-in helix. "helix/commands.scm"))
(require "helix/ext.scm")
(require-builtin helix/core/text as text.)
(require-builtin steel/process)
(require-builtin steel/json as json.)
(require-builtin steel/time)
(require "steel/result")

(provide zeta-accept
         zeta-dismiss
         zeta-enable
         zeta-disable
         zeta-toggle
         zeta-status)

;; ─────────────────────── Configuration ───────────────────────

(define *LM-STUDIO-URL* "http://localhost:1234/v1/completions")
(define *MODEL-NAME* "NexVeridian/zeta-2-4bit")
(define *MAX-TOKENS* 256)
(define *TEMPERATURE* 0.0)
(define *STOP-TOKENS* (list ">>>>>>> UPDATED" "<[end▁of▁sentence]>"))

;; How many lines of context around cursor for the CURRENT block
(define *CONTEXT-LINES-BEFORE* 80)
(define *CONTEXT-LINES-AFTER* 40)

;; Debounce delay in ms after last keystroke before requesting completion
(define *DEBOUNCE-MS* 400)

;; ─────────────────────── State ───────────────────────────────

;; Whether the plugin is active
(define *enabled* #t)

;; Current ghost text hint ID (list of first-line last-line), or #f
(define *current-hint-id* #f)

;; The pending completion text (what we'd insert on accept)
(define *pending-completion* #f)

;; Debounce generation counter
(define *request-gen* 0)

;; Whether a request is currently in-flight
(define *in-flight* #f)

;; ─────────────────────── Helpers ─────────────────────────────

(define (get-current-text)
  (let* ([focus (editor-focus)]
         [doc-id (editor->doc-id focus)])
    (text.rope->string (editor->text doc-id))))

(define (get-current-path)
  (let* ([focus (editor-focus)]
         [doc-id (editor->doc-id focus)])
    (editor-document->path doc-id)))

(define (get-cursor-pos)
  (cursor-position))

(define (get-current-line)
  (helix.static.get-current-line-number))

;;; Split text into lines
(define (string-split-lines str)
  (define (split-helper chars current acc)
    (cond
      [(null? chars) (reverse (cons (list->string (reverse current)) acc))]
      [(char=? (car chars) #\newline)
       (split-helper (cdr chars) '() (cons (list->string (reverse current)) acc))]
      [else (split-helper (cdr chars) (cons (car chars) current) acc)]))
  (split-helper (string->list str) '() '()))

;;; Join a list of strings with a separator
(define (string-join lst sep)
  (cond
    [(null? lst) ""]
    [(null? (cdr lst)) (car lst)]
    [else (string-append (car lst) sep (string-join (cdr lst) sep))]))

;;; Take up to n elements from a list
(define (take-up-to lst n)
  (cond
    [(<= n 0) '()]
    [(null? lst) '()]
    [else (cons (car lst) (take-up-to (cdr lst) (- n 1)))]))

;;; Escape a string for use in JSON
(define (json-escape str)
  (define chars (string->list str))
  (define (escape-char c)
    (cond
      [(char=? c #\\) "\\\\"]
      [(char=? c #\") "\\\""]
      [(char=? c #\newline) "\\n"]
      [(char=? c #\return) "\\r"]
      [(char=? c #\tab) "\\t"]
      [else (string c)]))
  (apply string-append (map escape-char chars)))

;;; Find index of substring, returns #f if not found
(define (string-index-of haystack needle)
  (define hlen (string-length haystack))
  (define nlen (string-length needle))
  (if (> nlen hlen)
      #f
      (let loop ([i 0])
        (cond
          [(> (+ i nlen) hlen) #f]
          [(string=? (substring haystack i (+ i nlen)) needle) i]
          [else (loop (+ i 1))]))))

;;; Replace all occurrences of a substring
(define (string-replace-all str old new)
  (define olen (string-length old))
  (if (= olen 0)
      str
      (let loop ([i 0] [acc ""])
        (cond
          [(> (+ i olen) (string-length str))
           (string-append acc (substring str i (string-length str)))]
          [(string=? (substring str i (+ i olen)) old)
           (loop (+ i olen) (string-append acc new))]
          [else
           (loop (+ i 1) (string-append acc (substring str i (+ i 1))))]))))

;; ─────────────────────── Prompt Building ─────────────────────

;;; Build the Zeta-2 SPM prompt from current buffer state.
;;;
;;; Format (from model card):
;;;   <[fim-suffix]>
;;;   code after editable region
;;;   <[fim-prefix]><filename>edit_history
;;;   (git diff or empty)
;;;
;;;   <filename>path/to/target_file.py
;;;   code before editable region
;;;   <<<<<<< CURRENT
;;;   code that
;;;   needs to<|user_cursor|>
;;;   be rewritten
;;;   =======
;;;   <[fim-middle]>
;;;
;;; Returns (list prompt text-up-to-cursor text-after-cursor)

(define (build-zeta-prompt text cursor-offset path)
  (define lines (string-split-lines text))
  (define total-lines (length lines))

  ;; Figure out which line the cursor is on
  (define cursor-line (get-current-line))

  ;; Define the editable region: a few lines around cursor
  (define edit-start (max 0 (- cursor-line 3)))
  (define edit-end (min total-lines (+ cursor-line 4)))

  ;; Prefix: everything before the editable region (bounded)
  (define prefix-start (max 0 (- edit-start *CONTEXT-LINES-BEFORE*)))
  (define prefix-lines (take-up-to (list-tail lines prefix-start)
                                   (- edit-start prefix-start)))
  (define prefix-text (string-join prefix-lines "\n"))

  ;; Suffix: everything after the editable region (bounded)
  (define suffix-lines (take-up-to (if (< edit-end total-lines)
                                       (list-tail lines edit-end)
                                       '())
                                   *CONTEXT-LINES-AFTER*))
  (define suffix-text (string-join suffix-lines "\n"))

  ;; Editable region (the CURRENT block)
  (define edit-region-lines
    (take-up-to (if (< edit-start total-lines)
                    (list-tail lines edit-start)
                    '())
                (- edit-end edit-start)))

  ;; Lines from edit-start up to AND including the cursor line
  ;; This is what we'll strip from the front of model output
  (define cursor-index-in-region (- cursor-line edit-start))
  (define lines-up-to-cursor (take-up-to edit-region-lines (+ cursor-index-in-region 1)))
  (define text-up-to-cursor (string-join lines-up-to-cursor "\n"))

  ;; Lines AFTER the cursor line within the edit region
  ;; This is what we'll strip from the tail of model output
  (define lines-after-cursor
    (if (< (+ cursor-index-in-region 1) (length edit-region-lines))
        (list-tail edit-region-lines (+ cursor-index-in-region 1))
        '()))
  (define text-after-cursor (string-join lines-after-cursor "\n"))

  ;; Now add <|user_cursor|> marker for the prompt
  (define (add-cursor-marker region-lines region-start-line)
    (let loop ([ls region-lines] [i region-start-line] [acc '()])
      (if (null? ls)
          (reverse acc)
          (if (= i cursor-line)
              (loop (cdr ls) (+ i 1)
                    (cons (string-append (car ls) "<|user_cursor|>") acc))
              (loop (cdr ls) (+ i 1) (cons (car ls) acc))))))

  (define edit-lines-with-cursor (add-cursor-marker edit-region-lines edit-start))
  (define edit-text (string-join edit-lines-with-cursor "\n"))

  ;; Filename for the prompt
  (define filename (or path "untitled"))

  ;; Build the full SPM prompt matching the model card exactly
  (define prompt
    (string-append
     "<[fim-suffix]>\n"
     suffix-text "\n"
     "<[fim-prefix]>"
     "<filename>edit_history\n"
     "\n"
     "<filename>" filename "\n"
     prefix-text "\n"
     "<<<<<<< CURRENT\n"
     edit-text "\n"
     "=======\n"
     "<[fim-middle]>"))

  ;; Return prompt + text-up-to-cursor + text-after-cursor
  (list prompt text-up-to-cursor text-after-cursor))

;; ─────────────────────── LM Studio Request ───────────────────

;;; Build the JSON payload for the LM Studio /v1/completions endpoint
(define (build-request-json prompt)
  (string-append
   "{"
   "\"model\":\"" (json-escape *MODEL-NAME*) "\","
   "\"prompt\":\"" (json-escape prompt) "\","
   "\"max_tokens\":" (int->string *MAX-TOKENS*) ","
   "\"temperature\":" (number->string *TEMPERATURE*) ","
   "\"stop\":[" (string-join (map (lambda (s) (string-append "\"" (json-escape s) "\""))
                                   *STOP-TOKENS*)
                              ",")
   "],"
   "\"stream\":false"
   "}"))

;;; Call LM Studio via curl in a subprocess, return the response body as a string.
;;; This is designed to run on a background thread.
(define (lm-studio-request prompt)
  (define json-body (build-request-json prompt))

  ;; Pipe JSON via stdin to curl to avoid temp files and shell escaping
  (define handle
    (~> (command "curl"
                 (list "-s"
                       "-X" "POST"
                       *LM-STUDIO-URL*
                       "-H" "Content-Type: application/json"
                       "-d" "@-"
                       "--max-time" "30"))
        with-stdin-piped
        with-stdout-piped
        spawn-process
        unwrap-ok))

  ;; Write request body to curl's stdin then close to signal EOF
  (define stdin-port (child-stdin handle))
  (display json-body stdin-port)
  (close-output-port stdin-port)

  (define result (read-port-to-string (child-stdout handle)))
  (wait handle)
  result)

;; ─────────────────────── Response Parsing ────────────────────

;;; Parse the completion text from the LM Studio JSON response.
;;; Strips >>>>>>> UPDATED and <|user_cursor|> markers.
(define (parse-completion response-str)
  (with-handler
   (lambda (err)
     (log::info! (to-string "zeta-complete: failed to parse response: " err))
     #f)
   (let* ([json (json.string->jsexpr response-str)]
          [choices (hash-ref json 'choices)]
          [first-choice (car choices)]
          [text (hash-ref first-choice 'text)])
     ;; Strip >>>>>>> UPDATED marker if present
     (let* ([idx (string-index-of text ">>>>>>> UPDATED")]
            [cleaned (if idx (substring text 0 idx) text)])
       ;; Strip <|user_cursor|> markers
       (string-replace-all cleaned "<|user_cursor|>" "")))))

;; ─────────────────────── Ghost Text Extraction ───────────────

;;; Given the model's full CURRENT block replacement, the text from the
;;; start of the CURRENT block up to the cursor, and the text after the
;;; cursor to the end of the CURRENT block, extract only the NEW content
;;; that should appear as ghost text.
;;;
;;; 1. Strip the pre-cursor prefix from the front
;;; 2. Strip the post-cursor suffix from the tail ONLY if it's an exact tail match
(define (extract-ghost-text completion text-up-to-cursor text-after-cursor)
  (define prefix-len (string-length text-up-to-cursor))
  (define comp-len (string-length completion))

  ;; Step 1: strip the front (text before/at cursor)
  (define after-prefix
    (if (and (>= comp-len prefix-len)
             (string=? (substring completion 0 prefix-len) text-up-to-cursor))
        ;; Clean case: model kept everything before cursor
        (substring completion prefix-len comp-len)
        ;; Model rewrote lines before cursor — find longest common prefix
        (let* ([max-check (min comp-len prefix-len)]
               [common-len
                (let loop ([i 0])
                  (if (and (< i max-check)
                           (char=? (string-ref completion i)
                                   (string-ref text-up-to-cursor i)))
                      (loop (+ i 1))
                      i))])
          (if (< common-len comp-len)
              (substring completion common-len comp-len)
              ""))))

  ;; Step 2: strip the tail ONLY if after-prefix ends exactly with text-after-cursor
  (define suffix-len (string-length text-after-cursor))
  (define after-len (string-length after-prefix))

  (if (and (> suffix-len 0)
           (>= after-len suffix-len)
           (string=? (substring after-prefix (- after-len suffix-len) after-len)
                     text-after-cursor))
      (substring after-prefix 0 (- after-len suffix-len))
      after-prefix))

;; ─────────────────────── Ghost Text Display ──────────────────

(define (clear-hint!)
  (when *current-hint-id*
    (with-handler
     (lambda (err)
       (log::info! (to-string "zeta-complete: failed to clear hint: " err)))
     (remove-inlay-hint-by-id (car *current-hint-id*) (cadr *current-hint-id*)))
    (set! *current-hint-id* #f)
    (set! *pending-completion* #f)))

(define (show-hint! text)
  (clear-hint!)
  (when (and (string? text) (> (string-length text) 0))
    (let ([trimmed (trim text)])
      (when (> (string-length trimmed) 0)
        (set! *pending-completion* trimmed)
        (set! *current-hint-id* (add-inlay-hint (get-cursor-pos) trimmed))))))

;; ─────────────────────── Core Logic ──────────────────────────

;;; Trigger a completion request. Runs the HTTP call on a background thread
;;; and shows the result as ghost text on the main thread.
(define (request-completion!)
  ;; Bump generation counter for debounce
  (set! *request-gen* (+ *request-gen* 1))
  (define my-gen *request-gen*)

  ;; Don't stack requests
  (when *in-flight*
    (return! void))

  (set! *in-flight* #t)

  ;; Capture context on the main thread
  (define text (get-current-text))
  (define cursor-offset (get-cursor-pos))
  (define path (get-current-path))

  ;; Build prompt on main thread
  (define prompt-pair (build-zeta-prompt text cursor-offset path))
  (define prompt (car prompt-pair))
  (define text-up-to-cursor (cadr prompt-pair))
  (define text-after-cursor (caddr prompt-pair))

  ;; Fire off background request
  (spawn-native-thread
   (lambda ()
     (define response
       (with-handler
        (lambda (err)
          (log::info! (to-string "zeta-complete: request error: " err))
          #f)
        (lm-studio-request prompt)))

     (set! *in-flight* #f)

     (when response
       (define completion (parse-completion response))
       (when (and completion (> (string-length completion) 0))
         ;; Extract only the part after the cursor position
         (define ghost (extract-ghost-text completion text-up-to-cursor text-after-cursor))
         ;; Only show if generation still matches (no new keystrokes since)
         (when (and (> (string-length ghost) 0) (= my-gen *request-gen*))
           (hx.with-context
            (lambda ()
              ;; Double check generation after acquiring context
              (when (= my-gen *request-gen*)
                (show-hint! ghost))))))))))

;; ─────────────────────── Commands ────────────────────────────

;;@doc
;; Accept the current ghost text completion, inserting it at the cursor.
(define (zeta-accept)
  (when *pending-completion*
    (let ([text *pending-completion*])
      (clear-hint!)
      (helix.static.insert_string text))))

;;@doc
;; Dismiss the current ghost text completion without inserting.
(define (zeta-dismiss)
  (clear-hint!))

;;@doc
;; Enable Zeta AI completions.
(define (zeta-enable)
  (set! *enabled* #t)
  (set-status! "Zeta completions enabled"))

;;@doc
;; Disable Zeta AI completions.
(define (zeta-disable)
  (set! *enabled* #f)
  (clear-hint!)
  (set-status! "Zeta completions disabled"))

;;@doc
;; Toggle Zeta AI completions on/off.
(define (zeta-toggle)
  (if *enabled* (zeta-disable) (zeta-enable)))

;;@doc
;; Show current Zeta status in the status line.
(define (zeta-status)
  (set-status! (string-append "Zeta: "
                              (if *enabled* "enabled" "disabled")
                              (if *in-flight* " (loading...)" "")
                              (if *pending-completion* " [suggestion ready]" ""))))

;; ─────────────────────── Hooks ───────────────────────────────

;;; After a character is inserted, schedule a debounced completion request.
(register-hook! "post-insert-char"
                (lambda (_)
                  (when *enabled*
                    ;; Clear any existing hint on new input
                    (clear-hint!)
                    ;; Bump generation to invalidate any in-flight request
                    (set! *request-gen* (+ *request-gen* 1))
                    (define my-gen *request-gen*)
                    ;; Debounce: request after a delay
                    (enqueue-thread-local-callback-with-delay
                     *DEBOUNCE-MS*
                     (lambda ()
                       ;; Only fire if no newer keystrokes happened
                       (when (and *enabled* (= my-gen *request-gen*))
                         (request-completion!)))))))

;;; On any command (cursor movement, mode switch, etc.), clear the hint.
(register-hook! "post-command"
                (lambda (cmd)
                  (when *enabled*
                    ;; Don't clear on insert-char since we handle that above
                    (unless (string=? (to-string cmd) "insert_char")
                      (clear-hint!)))))
