(module matlab *
(import chicken scheme extras foreign)
(use traversal bind easyffi lolevel define-structure linear-algebra srfi-1 scheme2c-compatibility)

;;; TODO Scheme to matlab conversion and proper matlab function calls
;;; TODO Matlab cell, struct, logical and function
;;; TODO Support for building matlab structures
;;; TODO Support for complex numbers

#>
#include <engine.h>
<#

;; The current matlab engine
(define *matlab-engine* #f)

(define *matlab-class-unknown*  (foreign-value "mxUNKNOWN_CLASS" int))
(define *matlab-class-cell*     (foreign-value "mxCELL_CLASS" int))
(define *matlab-class-struct*   (foreign-value "mxSTRUCT_CLASS" int))
(define *matlab-class-logical*  (foreign-value "mxLOGICAL_CLASS" int))
(define *matlab-class-char*     (foreign-value "mxCHAR_CLASS" int))
(define *matlab-class-double*   (foreign-value "mxDOUBLE_CLASS" int))
(define *matlab-class-single*   (foreign-value "mxSINGLE_CLASS" int))
(define *matlab-class-int8*     (foreign-value "mxINT8_CLASS" int))
(define *matlab-class-uint8*    (foreign-value "mxUINT8_CLASS" int))
(define *matlab-class-int16*    (foreign-value "mxINT16_CLASS" int))
(define *matlab-class-uint16*   (foreign-value "mxUINT16_CLASS" int))
(define *matlab-class-int32*    (foreign-value "mxINT32_CLASS" int))
(define *matlab-class-uint32*   (foreign-value "mxUINT32_CLASS" int))
(define *matlab-class-int64*    (foreign-value "mxINT64_CLASS" int))
(define *matlab-class-uint64*   (foreign-value "mxUINT64_CLASS" int))
(define *matlab-class-function* (foreign-value "mxFUNCTION_CLASS" int))

(define *matlab-sizeof-mwSize* (foreign-type-size "mwSize"))

;;; Roughly the number of elements a vector can have when sent over
;;; the matlab string interface, anything bigger than this must be
;;; written to a file first; the actual number seems to be close to
;;; 200. I have no idea how to increase the size of the string buffer
;;; that matlab uses

(define *matlab-largest-string* 150)

;;; Matlab engine bindings

(define matlab-start (foreign-lambda c-pointer "engOpen" c-string))
(define matlab-stop (foreign-lambda int "engClose" c-pointer))
(define matlab-variable
 (foreign-lambda c-pointer "engGetVariable" c-pointer c-string))
(define matlab-variable-set!
 (foreign-lambda int "engPutVariable" c-pointer c-string c-pointer))
(define c-matlab-eval-string (foreign-lambda int "engEvalString" c-pointer c-string))
(define matlab-set-output-buffer
 (foreign-lambda int "engOutputBuffer" c-pointer c-pointer int))
(define matlab-set-visible (foreign-lambda int "engSetVisible" c-pointer bool))
(define matlab-get-visible (foreign-lambda int "engGetVisible" c-pointer c-pointer))

(define (matlab-eval-string s) (start-matlab!) (c-matlab-eval-string *matlab-engine* s))

(define (matlab-eval-strings . strings)
 (start-matlab!) (for-each (lambda (s) (matlab-eval-string s)) strings))

(define (matlab . strings)
 (start-matlab!)
 (with-matlab-default-output-buffer
  (lambda (matlab-result-string)
   (apply matlab-eval-strings strings)
   (matlab-result-string))))

(define *default-matlab-engine-command*
 (string-append
  "matlab -nosplash -nodesktop "
  (let ((args (getenv "MATLAB_LOCAL_ARGS"))) (if  args args ""))))
(define *default-matlab-buffer-size* 5000)

(define (with-matlab-default-engine f)
 (with-matlab-engine *default-matlab-engine-command* f))

(define (with-matlab-engine str f)
 (set! *matlab-engine* (matlab-start str))
 (let ((result (f)))
  (matlab-stop *matlab-engine*)
  result))

(define (with-matlab-default-output-buffer f)
 (with-matlab-output-buffer *default-matlab-buffer-size* f))

(define (with-matlab-output-buffer size f)
 (with-alloc
  size
  (lambda (buffer)
   (matlab-set-output-buffer *matlab-engine* buffer size)
   (let ((result (f (lambda () (c-string->string buffer)))))
    (matlab-set-output-buffer *matlab-engine* (address->pointer 0) 0)
    result))))

(define (start-matlab!)
 (unless *matlab-engine*
  (set! *matlab-engine* (matlab-start *default-matlab-engine-command*))))

;;; Matlab MX Array bindings

(define matlab-class-id (foreign-lambda int "mxGetClassID" c-pointer))
(define matlab-class-name (foreign-lambda c-string "mxGetClassName" c-pointer))
(define matlab-array->string (foreign-lambda c-string "mxArrayToString" c-pointer))
(define matlab-calloc (foreign-lambda c-pointer "mxCalloc" int int))
(define matlab-malloc (foreign-lambda c-pointer "mxMalloc" int))
(define matlab-free (foreign-lambda void "mxFree" c-pointer))
(define matlab-make-numeric-matrix
 (foreign-lambda c-pointer "mxCreateNumericMatrix" int int int int))
(define matlab-destroy (foreign-lambda void "mxDestroyArray" c-pointer))
(define matlab-nr-rows (foreign-lambda int "mxGetM" c-pointer))
(define matlab-nr-columns (foreign-lambda int "mxGetN" c-pointer))
(define matlab-nr-dimensions
 (foreign-lambda int "mxGetNumberOfDimensions" c-pointer))
(define matlab-nr-elements (foreign-lambda int "mxGetNumberOfElements" c-pointer))
(define matlab-dimensions-internal
 (foreign-lambda c-pointer "mxGetDimensions" c-pointer))
(define matlab-data-set! (foreign-lambda void "mxSetData" c-pointer c-pointer))
(define matlab-data (foreign-lambda c-pointer "mxGetData" c-pointer))
(define matlab-real-double-data (foreign-lambda c-pointer "mxGetPr" c-pointer))
(define matlab-real-double-data-set!
 (foreign-lambda void "mxSetPr" c-pointer c-pointer))
(define matlab-complex-double-data (foreign-lambda c-pointer "mxGetPi" c-pointer))
(define matlab-complex-double-data-set!
 (foreign-lambda void "mxSetPi" c-pointer c-pointer))
(define matlab-element-size (foreign-lambda int "mxGetElementSize" c-pointer))
(define matlab-calculate-index-internal
 (foreign-lambda int "mxCalcSingleSubscript" c-pointer int c-pointer))

(define (matlab-dimensions matrix)
 (c-exact-array->list (matlab-dimensions-internal matrix)
		      *matlab-sizeof-mwSize*
		      (matlab-nr-dimensions matrix)
		      #f))

(define (matlab-calculate-index matrix array subscripts)
 (matlab-calculate-index-internal
  matrix
  (length subscripts)
  (list->c-array array subscripts *matlab-sizeof-mwSize* #f)))

(define (matlab-string? m)
 (= *matlab-class-char* (matlab-class-id m)))

(define (matlab-matrix-exact? m)
 (eq? (car (matlab-matrix-numeric-type m)) 'exact))
(define (matlab-matrix-inexact? m)
 (eq? (car (matlab-matrix-numeric-type m)) 'inexact))
(define (matlab-matrix-signed? m)
 (eq? (cadr (matlab-matrix-numeric-type m)) 'signed))
(define (matlab-matrix-unsigned? m)
 (eq? (cadr (matlab-matrix-numeric-type m)) 'unsigned))

(define (matlab-matrix-numeric-type m)
 (let ((id (matlab-class-id m)))
  (cond
   ((or (= *matlab-class-double* id)
	(= *matlab-class-single* id))
    `(inexact signed))
   ((or (= *matlab-class-int8* id)
	(= *matlab-class-int16* id)
	(= *matlab-class-int32* id)
	(= *matlab-class-int64* id))
    `(exact signed))
   ((or (= *matlab-class-uint8* id)
	(= *matlab-class-uint16* id)
	(= *matlab-class-uint32* id)
	(= *matlab-class-uint64* id))
    `(exact unsigned))
   (else '(#f #f)))))

;;; Various handy utilities

(define (matlab-size-ref struct x)
 ((c-sized-int-ptr-ref *matlab-sizeof-mwSize* #f)
  struct (* *matlab-sizeof-mwSize* x)))

(define (matlab-matrix->list matrix signed?)
 (vector->list (matlab-matrix->matrix matrix signed?)))

(define (matlab-matrix->matrix matrix signed?)
 (let ((size (matlab-element-size matrix)))
  (matlab-data->vector
   (cond ((matlab-matrix-exact? matrix) (matlab-real-double-data matrix))
	 ((matlab-matrix-inexact? matrix) (matlab-real-double-data matrix))
	 (else (fuck-up)))
   (cond ((matlab-matrix-exact? matrix)
	  (lambda (array nr-elements)
	   (c-exact-array->vector array size nr-elements signed?)))
	 ((matlab-matrix-inexact? matrix)
	  (lambda (array nr-elements)
	   (c-inexact-array->vector array size nr-elements signed?)))
	 (else (fuck-up)))
   (matlab-element-size matrix)
   (reverse (matlab-dimensions matrix)))))

(define (matlab-data->vector data get-row element-size dimensions)
 (cond
  ((= 1 (length dimensions))
   (get-row data (car dimensions)))
  ((< 1 (length dimensions))
   (matrix-transpose
    (map-n-vector
     (lambda (n)
      (matlab-data->vector
       (pointer+ data (* n element-size (apply * (cdr dimensions))))
       get-row element-size (cdr dimensions)))
     (car dimensions))))
  (else (fuck-up))))

(define (matlab-list-output t . file)
 (if (null? file)
     (matlab-matrix-output (list->vector (map list->vector t)))
     (matlab-matrix-output (list->vector (map list->vector t)) (car file))))

(define (matlab-matrix-output t . file)
 ((if (null? file)
      (lambda (f) (f #t))
      (lambda (f) (call-with-output-file (first file) f)))
  (lambda (port)
   (let loop ((a t))
    (for-each-vector (lambda (a) (if (vector? a)
				(begin (loop a) (format port "~%"))
				(format port " ~a" a)))
		     a)))))

(define (scheme-vector->matlab-string v)
 (string-append
  "["
  (foldl
   (lambda (i e)
    (string-append
     i
     ", "
     (number->string e)))
   (number->string (vector-ref v 0))
   (cdr (vector->list v)))
  "]"))

(define (scheme->matlab! variable s)
 (cond
  ;; ((pnm? s)
  ;;  (with-temporary-file
  ;;   (cond ((pbm? s) "matlab.pbm")
  ;;       	  ((pgm? s) "matlab.pgm")
  ;;       	  ((ppm? s) "matlab.ppm"))
  ;;   (lambda (f)
  ;;    (write-pnm s f)
  ;;    (matlab-eval-string
  ;;     (format #f "~a=imread('~a');" variable f)))))
  ((and (matrix? s)
		(< (* (matrix-rows s) (matrix-columns s))
		   *matlab-largest-string*))
   (let ((vec (map-vector scheme-vector->matlab-string s)))
    (matlab-eval-string
     (format #f
			 "~a=~a"
			 variable
			 (string-append
			  "["
			  (foldl
			   (lambda (j f) (string-append j "; " f))
			   (vector-ref vec 0)
                           (cdr (vector->list vec)))
			  "]")))))
  ((and (vector? s) (not (matrix? s))
		(< (vector-length s) *matlab-largest-string*))
   (matlab-eval-string
    (format #f
			"~a=~a;"
			variable
			(scheme-vector->matlab-string s))))
  ((vector? s)
   (with-temporary-file
    "/tmp/matlabmatrix.m"
    (lambda (f)
     (matlab-matrix-output s f)
     (matlab-eval-string
      (format #f "~a = importdata('~a', ' ');" variable f)))))
  ((list? s)
   (scheme->matlab! variable (list->vector s)))
  (else (fuck-up))))

(define (matlab->scheme variable-name)
 (cond ((or (matlab-matrix-exact? variable-name) (matlab-matrix-inexact? variable-name))
        (matlab-matrix->matrix variable-name (matlab-matrix-signed? variable-name)))
       ((or (matlab-string? variable-name)) (matlab-array->string variable-name))
       (else (fuck-up))))

(define (with-matlab-variable var f)
 (let* ((x (matlab-variable *matlab-engine* var))
		(result (f x)))
  (matlab-destroy x)
  result))

(define (matlab-get-variable name)
 (with-matlab-variable name (lambda (var) (matlab->scheme var))))

(define (matlab-get-double name)
 (with-matlab-variable name (lambda (var) (x (x (matlab->scheme var))))))

(define (matlab-save-variables filename . variables)
 (matlab-eval-string
  (format #f "save ~a ~a"
		  filename
		  (qreduce
		   string-append
		   (map (lambda (v) (format #f "~a " v)) variables)
		   ""))))

(define (matlab-load-variables filename)
 (matlab-eval-string (format #f "load ~a" filename)))

(define (matlab-show-variable name)
 (with-matlab-variable
  name
  (lambda (var)
   (format #t "~a is ~a~%" name (matlab->scheme var)))))

(define (matlab-append-to-path directory)
 (matlab-eval-string
  (format "addpath(genpath('~a'))" directory)))
)
