;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(delete-other-windows "a")
(window-buffer)
(window-list)

major-mode
initial-major-mode
(stringp 'system-type)
(list-faces-display)
get-text-property
(text-properties-at 5)
(zerop nil)
?q

(internal-set-font-selection-order '(:slant :weight :width :height))
font-family-list
(keywordp :height)
set-face-attribute

(type-of :height)
face-font-selection-order
set-face-font-sort-order

face-new-frame-defaults
system-type
delq

error

(sort (face-list) #'string-lessp)

face-font-selection-order

face-font-selection-order

(defcustom katefro
  '(:width :height :weight :slant)
  "A list specifying how face font selection chooses fonts.
Each of the four symbols `:width', `:height', `:weight', and `:slant'
must appear once in the list, and the list must not contain any other
elements.  Font selection first tries to find a best matching font
for those face attributes that appear before in the list.  For
example, if `:slant' appears before `:height', font selection first
tries to find a font with a suitable slant, even if this results in
a font height that isn't optimal."
  :tag "Font selection order"
  :type '(list symbol symbol symbol symbol)
  :group 'font-selection
  :set #'(lambda (symbol value)
	   (set-default symbol value)
	   (internal-set-font-selection-order value)))

(symbol-value 'katefro)

(defcustom katef
  (mapcar (lambda (arg) (mapcar 'purecopy arg))
  '(("Monospace" "courier" "fixed")
    ("courier" "CMU Typewriter Text" "fixed")
    ("Sans Serif" "helv" "helvetica" "arial" "fixed")
    ("helv" "helvetica" "arial" "fixed")))
  "Alist of alternative font family names.
Each element has the form (FAMILY ALTERNATIVE1 ALTERNATIVE2 ...).
If fonts of family FAMILY can't be loaded, try ALTERNATIVE1, then
ALTERNATIVE2 etc."
  :tag "Alternative font families to try"
  :type '(repeat (repeat string))
  :group 'font-selection
  :set #'(lambda (symbol value)
	   (set-default symbol value)
	   (internal-set-alternative-font-family-alist value)))

(symbol-value 'katef)
(internal-set-alternative-font-family-alist '((1)))

custom-initialize-reset
custom-declare-variable

internal-set-alternative-font-registry-alist
(internal-lisp-face-p '(1 2) nil)
defface