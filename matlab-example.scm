(use matlab)

(matlab "'hello'")
(matlab "a=2+2")
(matlab-get-variable "a")
(scheme->matlab! "a" '(3))
(matlab-get-variable "a")

;; fancy examply where we inspect the output buffer manually

(with-matlab-default-output-buffer
 (lambda (matlab-result-string)
  (matlab-eval-string "A=[[1,2,3];[4,5,6]]")
  (matlab-eval-string "B='Hi'")
  (matlab-eval-string "C=1")
  (matlab-show-variable "A")
  (matlab-show-variable "B")
  (matlab-show-variable "C")
  (matlab-eval-string "D=randn(2,2,2,2)")
  (display (matlab-result-string))
  (matlab-show-variable "D")
  (matlab-eval-string "'Can also get the output from matlab'")
  (display (matlab-result-string))
  (newline)))
