;;;; package.lisp

(defpackage #:weblocks-cms-logging
  (:use #:cl)
  (:export #:log-error))

(in-package :weblocks-cms-logging)

(weblocks-cms:def-additional-schema 
  :template
  `((:TITLE "Log Record" :NAME :LOG-RECORD :FIELDS
     ((:TITLE "Title" :NAME :TITLE :TYPE :STRING :OPTIONS NIL)
      (:TITLE "Session Id" :NAME :SESSION-ID :TYPE :STRING :OPTIONS NIL)
      (:TITLE "Data" :NAME :DATA :TYPE :CUSTOM :OPTIONS "weblocks-cms-logging::log-record-data-fields")
      (:TITLE "Time Created" :NAME :TIME-CREATED :TYPE :DATETIME :OPTIONS NIL)))))
 
