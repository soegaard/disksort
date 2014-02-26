;;; sort.scm  --  Jens Axel Søgaard

;;; PURPOSE

; This file contains routines for sorting files larger than main memory.

;;; DEPENDCIES

; Relies on vector-sort! from srfi-32 available from v35x.

;;; USAGE

; The file format is simply
;    <record><record><record>...

; The user provides read-record, which reads a <record>, and
; write-record that writes a record.

; The main function is:

;  (disk-sort in-file out-file block-size read-record write-record less?)

; This will take the records in in-file, sort them according to less?,
; and write them to out-file.
; The block-size indicates the number of records that can be sorted in-memory.
; It is okay to let in-file and out-file be the same.

;;; IMPLEMENTATION

(module sort mzscheme
  (provide disk-sort)
  
  (require (only (lib "43.ss" "srfi") 
                 vector-copy vector-for-each))
  (require (only (lib "32.ss" "srfi")
                 vector-sort!))
  

  
  (define (read-block-from-file filename position block-size read-record)
    (let ([port (open-input-file filename)])
      (begin0
        (read-block port position block-size read-record)
        (close-input-port port))))
  
  (define (read-block port position block-size read-record)
    ; Read size records from port starting from the file-position position
    ; Return the records as a vector. If there are less than size records
    ; left in the file, the vector returned will have a length smaller than size.
    (let ([v    (make-vector block-size)])
      (file-position port position)
      (begin0
        (let loop ([n 0])
          (cond
            [(= n block-size) v]
            [else             (let ([r (read-record port)])
                                (if (and (eof-object? r) (< n block-size))
                                    ; n < size   records read
                                    (vector-copy v 0 n)
                                    ; store read record, and read the rest
                                    (begin
                                      (vector-set! v n r)
                                      (loop (+ n 1)))))])))))
  
  (define (merge-vectors v1 v2 less?)
    (let* ([s1 (vector-length v1)]
           [s2 (vector-length v2)]
           [v  (make-vector (+ s1 s2))])
      (let loop ([n1 0] [n2 0] [n 0])
        (cond
          [(and (= n1 s1) (= n2 s2))    v]
          [(= n1 s1)                    (begin
                                          (vector-set! v n (vector-ref v2 n2))
                                          (loop n1 (+ n2 1) (+ n 1)))]
          [(= n2 s2)                    (begin
                                          (vector-set! v n (vector-ref v1 n1))
                                          (loop (+ n1 1) n2 (+ n 1)))]
          [(less? (vector-ref v1 n1)
                  (vector-ref v2 n2))   (begin
                                          (vector-set! v n (vector-ref v1 n1))
                                          (loop (+ n1 1) n2 (+ n 1)))]
          [else                         (begin
                                          (vector-set! v n (vector-ref v2 n2))
                                          (loop n1 (+ n2 1) (+ n 1)))]))))
  
  
  (define (write-block vector write-record port)
    (vector-for-each (lambda (i r) (write-record r port))
                     vector))
  
  
  (define (merge-blocks-in-file-once in-file out-file block-size read-record write-record less?)
    ; in-file consists of a series of blocks. Each block contains block-size records,
    ; in sorted order according to less?.
    
    ; out-file will consist of a series of blocks of size 2*block_size. Each sorted
    ; according to less?
    
    ; in-file and out-file can be the same file
    (let ([in  (open-input-file in-file)]
          [out (open-output-file out-file 'update)])
      
      (define (read-next-block)
        (read-block in (file-position in) block-size read-record))
      
      (begin0
        (let loop ([b1 (read-next-block)] 
                   [b2 (read-next-block)])
          (if (zero? (+ (vector-length b1) (vector-length b2)))
              (void)
              (begin
                (write-block (merge-vectors b1 b2 less?) write-record out)
                (loop (read-next-block) (read-next-block)))))
        (close-input-port in)
        (close-output-port out))))
  
  (define (filename->number-of-records filename read-record)
    (let ([in (open-input-file filename)])
      (begin0
        (let loop ([n 0])
          (if (eof-object? (read-record in))
              n
              (loop (+ n 1))))
        (close-input-port in))))
  
  (define (number-of-merges-needed size block-size)
    ; how many merges is neccessary to sort a file consisting of size records,
    ; the file consists of blocks of block-size sorted records
    (if (>= block-size size)
        0
        (+ 1 (number-of-merges-needed size (* 2 block-size)))))
  
  
  (define (merge-blocks-in-file file size block-size read-record write-record less?)
    (let ([in  (open-input-file file)]
          [out (open-output-file file 'update)])
      (begin0
        (if (zero? (number-of-merges-needed size block-size))
            (void)
            (begin
              (merge-blocks-in-file-once file file block-size read-record write-record less?)
              (merge-blocks-in-file file size (* 2 block-size) read-record write-record less?)))
        (close-input-port in)
        (close-output-port out))))
  
  
  (define (disk-sort in-file out-file block-size read-record write-record less?)
    ; TODO: Remember size from the first pass
    
    ; sort the records in in-filename.
    ; block-size records can be sorted in-memory
    (let ([in  (open-input-file in-file)]
          [out (open-output-file out-file 'replace)])
      (define (read-next-block)
        (read-block in (file-position in) block-size read-record))
      ; Initial pass reads blocks and sort them in-memory
      (let loop ([b (read-next-block)])
        (if (zero? (vector-length b))
            (begin
              (close-input-port in)
              (close-output-port out))
            (begin
              (vector-sort! less? b)
              (write-block b write-record out)
              (loop (read-next-block)))))
      ; Finish off by repeated merging
      (let ([size (filename->number-of-records out-file read-record)])
        (merge-blocks-in-file out-file size block-size read-record write-record less?))))
  
  
  ;;;
  ;;; TESTS
  ;;;

  (define (read-all-records-in-file file read-record)
    (let ([in (open-input-file file)])
      (begin0
        (let loop ([rs '()] [r (read-record in)])
          (if (eof-object? r)
              (list->vector (reverse! rs))
              (loop (cons r rs) (read-record in))))
        (close-input-port in))))
  
  
  (define (test-read-block)
    (and (equal? (read-block-from-file "test-7-numbers.txt" 0 4 read)
                 (vector 1 2 3 4))
         (equal? (read-block-from-file "test-7-numbers.txt" 0 10 read)
                 (vector 1 2 3 4 5 6 7))
         (equal? (read-block-from-file "test-7-numbers.txt" 4 2 read)
                 (vector 3 4))))
  
  (define (test-merge-vectors)
    (let ([v1 (vector 1 2   5 6  9 10)]
          [v2 (vector 0 0 3 4 7 8 9)])
      (and (equal? (merge-vectors v1 v2 <)
                   (vector 0 0 1 2 3 4 5 6 7 8 9 9 10))
           (equal? (merge-vectors (vector) v2 <)
                   v2)
           (equal? (merge-vectors v1 (vector) <)
                   v1)
           (equal? (merge-vectors (vector) (vector) <)
                   (vector)))))
  
  (require (only (lib "list.ss") sort))
  
  (define (test-merge-blocks-in-file)
    (define (read-record port)
      (let ([r (read port)])
        (if (eof-object? r)
            r
            (string->number r))))
    (define (write-record n port)
      (write (number->string n) port))
    (let ([filename "testing123.txt"]
          [v        (vector 3 8 0 2 -1)])
      (let ([out (open-output-file filename 'replace)])
        (write-block v write-record out)
        (close-output-port out))
      (merge-blocks-in-file filename
                            (filename->number-of-records filename read-record) 1 
                            read-record write-record <)
      (let ([in (open-input-file filename)])
        (begin0
          (let ([s (read-block in 0 (vector-length v) read-record)])
            (newline) (newline)
            (write s)
            
            (equal? (vector->list s)
                    (sort (vector->list v) <)))
          (close-input-port in)))))
  
  (define (test-disk-sort)
    (define (read-record port)
      (let ([r (read port)])
        (cond
          [(eof-object? r) r]
          [(string? r)     (string->number r)]
          [(number? r)     r]
          [else            (error)])))
    (define (write-record n port)
      (write (number->string n) port))
    
    (disk-sort "unsorted.txt" "testing123.txt"
               10 read-record write-record <)
    
    (equal? (sort (vector->list (read-all-records-in-file "unsorted.txt" read-record)) <)
            (vector->list (read-all-records-in-file "testing123.txt" read-record))))
  
  
  (define (test-all)
    (and (test-read-block)
         (test-merge-vectors)
         (test-merge-blocks-in-file)
         (test-disk-sort)))
  ; (test-all)
  
  )