
;; feed args so that jqt spawns socket server.
(defvar *jqt*)
(defvar *jcli*)

(defun start-jqt-client ()
  "emacs network process that communicates with a jqt server"
  (setq *jcli*
	(open-network-stream "jqt-client"
			     "*jqt-client*"
			     "127.0.0.1"
			     8017
			     :type 'plain
			     :nowait t)))


(defun spawn-jqt ()
  "spawn jqt as asyncronous process, feeding it a file that
starts a socket server"
  (setq *jqt*
	(async-start-process "jqt-server"
			     "jqt"
			     (lambda (obj)
			       (princ "jqt done"))
			     "jqt-server.ijs")))

(defun close-jqt ()
  "close the async jqt process"
  (delete-process *jqt*))

(defun stop-jqt-client ()
  "stop jqt client if there"
  (delete-process *jcli*))

(spawn-jqt)
(close-jqt)

(stop-jqt-client)

(list-processes)
(start-jqt-client)
(send-string *jcli* "?~ i. 10")


