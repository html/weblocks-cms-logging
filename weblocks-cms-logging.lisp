;;;; weblocks-cms-logging.lisp

(in-package #:weblocks-cms-logging)

(defmacro ignore-and-log-errors (log-function &body body)
  `(handler-case 
     (progn ,@body)
     (error (e) (funcall ,log-function e))))

;;; "weblocks-cms-logging" goes here. Hacks and glory await!
(defun log-error (condition &key on-error)
  "on-error - function which should be called when error occures during log attempt"
  (ignore-and-log-errors 
    on-error
    (firephp:fb weblocks-stores:*default-store* (make-instance 'weblocks-cms::log-record 
                     :title "Error caught"
                     :data (list :trace 
                                 #-sbcl(trivial-backtrace:print-backtrace condition :output nil)
                                 #+sbcl(sb-debug:backtrace-as-list))
                     :time-created (get-universal-time)))
    (weblocks-stores:persist-object 
      weblocks-stores:*default-store* 
      (make-instance 'weblocks-cms::log-record 
                     :title "Error caught"
                     :session-id (weblocks::session-cookie-value weblocks::*session*)
                     :data (list 
                             :error-class-name (class-name (class-of condition))
                             :error-string (format nil "~A" condition)
                             :trace 
                                 #-sbcl(trivial-backtrace:print-backtrace condition :output nil)
                                 #+sbcl(loop for i in (sb-debug:backtrace-as-list) 
                                            collect (list (first i) 
                                                          (mapcar #'prin1-to-string (cdr i)))))
                     :time-created (get-universal-time)))))

(defmacro yaclml->string (&body body)
  `(yaclml:with-yaclml-output-to-string ,@body))

(defun log-record-data-fields (type description model-description-list)
  (case type 
    (:form (list 
             (list 
               (weblocks-cms::keyword->symbol (getf description :name))
               :present-as 'html
               :reader (lambda (item)
                         (yaclml->string 
                           (<:dl 
                             (<:dt "Title")
                             (<:dd (<:as-is (weblocks-cms::log-record-title item)))
                             (<:dt "Time")
                             (<:dd (<:as-is (metatilities:format-date "%d.%m.%Y %I:%M" (weblocks-cms::log-record-time-created item))))
                             (<:dt "Error")
                             (<:dd (<:b (<:as-is (getf (weblocks-cms::log-record-data item) :error-class-name)))
                                   (<:as-is "&nbsp;")
                                   (<:as-is (getf (weblocks-cms::log-record-data item) :error-string)))
                             (<:dt "Backtrace")
                             (<:dd 
                               (<:table :class "table table-striped" :style "font-size:smaller"
                                 (<:thead 
                                   (<:tr 
                                     (<:th "Function")
                                     (<:th "Arguments")))
                                 (<:tbody 
                                   (loop for (func args) in (getf (weblocks-cms::log-record-data item) :trace) do 
                                         (<:tr 
                                           (<:td :style "white-space:nowrap"
                                             (<:format "~A" func))
                                           (<:td 
                                             (loop for argument in args do 
                                                   (<:format "~A" argument)
                                                   (<:br))))))))))))))
    (:table (list 
              (list 
                (weblocks-cms::keyword->symbol (getf description :name))
                :hidep t)))))
