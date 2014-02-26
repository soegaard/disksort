disksort
========

Sort large files

----------------------------------------------------------------------
  _DISK SORT_                                   Jens Axel Søgaard
----------------------------------------------------------------------

----------------------------------------------------------------------
HISTORY
----------------------------------------------------------------------
2006-oct-15  First release: disk-sort

----------------------------------------------------------------------
INTRODUCTION
----------------------------------------------------------------------

This library provides implementations of various disk related
algorithms. 


----------------------------------------------------------------------
_DISK-SORT_
----------------------------------------------------------------------

> (disk-sort in-file out-file block-size read-record write-record less?)

Sort the records in in-file and store the result in out-file.
The file format of both in-file and out-file is simply

    <record><record><record>...

where the exact format of <record> is determined by the user
functions read-record and write-record. The records do not
have to be of the same length.

Disk-sort can handle file larger than main-memory. The 
block-size is the number of records that can be sorted
in main memory. 

It is okay for in-file and out-file to be the same 
filename.

The records are sorted according to less?.

> read-record : port -> record
> write-record : record port -> 
> less? : record record -> boolean


EXAMPLE (sorting a space-seperated file of strings)
---------------------------------------------------
The strings are assumed to be enclosed in "s.

(disk-sort "unsorted.txt" "sorted.txt" 10000 
           read write string<?)


EXAMPLE (sorting a space-seperated file of integers)
----------------------------------------------------
(define (read-record port) 
  (read port))

(define (write-record number port)
  (write number port)
  (display " " port))

(disk-sort "unsorted.txt" "sorted.txt" 10000 
           read-record write-record <)


Keywords: _disk_ _sort_ _sorting_ _record_ _records_