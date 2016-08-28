;;;; package.lisp

(defpackage #:weblocks-cms-logging
  (:use #:cl)
  (:export #:log-error))

(in-package :weblocks-cms-logging)

(weblocks-cms:def-additional-schema 
  :log-record
  `((:TITLE "Log Record" :NAME :LOG-RECORD :FIELDS
     ((:TITLE "Title" :NAME :TITLE :TYPE :STRING :OPTIONS NIL)
      (:TITLE "Session Id" :NAME :SESSION-ID :TYPE :STRING :OPTIONS NIL)
      (:TITLE "Data" :NAME :DATA :TYPE :CUSTOM :OPTIONS "weblocks-cms-logging::log-record-data-fields")
      (:TITLE "Time Created" :NAME :TIME-CREATED :TYPE :DATETIME :OPTIONS NIL)))))
 
(defmacro ignore-and-log-errors (log-function &body body)
  `(handler-case 
     (progn ,@body)
     (error (e) (funcall ,log-function e))))
