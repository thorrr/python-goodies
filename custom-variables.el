;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizable variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom python-column-width 80
  "Set fill-column to this value in python files.  Also, if using
  pep8, warn when columns exceed this value"
  :type 'integer
  )

(defcustom pymacs-parent-dir nil
  "Set this if you have pymacs/ropemacs installed"
  :type 'directory
  )

(defcustom python-use-pyflakes nil
  "Use pyflakes with flymake"
  :type 'boolean)

(defcustom python-use-pep8 nil
  "Use pep8 with flymake"
  :type 'boolean)

(defcustom python-pep8-options "--ignore=E124,E265,E701,E702,E129,E114,E116"
  "pep8 command line options"
   :type 'string)

(defcustom python-use-pylint nil
  "Use pylint with flymake"
  :type 'boolean)

(defcustom python-pylint-options  "--output-format=parseable --reports=n\
                                   --extension-pkg-whitelist=numpy\
                                   --disable=I0011,R0913,C0103,C0302,C0111,W0511,C0325"
  "pylint command line options"
  :type 'string)

(defcustom auto-python-just-source-file nil
  "Automatically run python-just-source-file periodically"
  :type 'boolean)

(defcustom auto-detect-virtualenv nil
  "When loading a python file attempt to find its virtualenv using function detect-virtualenv.")
