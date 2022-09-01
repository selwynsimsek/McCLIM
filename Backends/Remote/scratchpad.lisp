
(setf lfarm:*kernel* (lfarm-client:make-kernel '(("localhost" 7159))))
(defvar *ch* (lfarm:make-channel))

(defun submit-sync ()
  (lfarm:submit-task *ch* #'+ 1 2 3)
  (lfarm:receive-result *ch*)
  ;(lfarm:try-receive-result *ch*)
  )

(defun booyakashalaka ()
  (lfarm:submit-task *ch* #'mcclim-sdl2::create-window "Hello world" :synchronize t )
  (lfarm:receive-result *ch*))

(defun clear-lfarm-queue ()
  (loop for i from 0
        while (lfarm:try-receive-result *ch*)
        finally (return i)))

(clear-lfarm-queue)

(let ((channel ))
                                        
  )


