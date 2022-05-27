(in-package #:cl-twitch)

(defparameter *db-file*
  (merge-pathnames (asdf:component-pathname (asdf:find-system :cl-twitch))
                   #P"twitch.db"))

(defun create-table (name &rest fields)
  (declare (type (or string symbol keyword) name))
  (let ((fields (append (list '(id :integer primary key)) fields)))
    (format NIL "create table ~(~a~) (~(~{~{~a~^ ~}~^, ~}~))" name fields)))

(defun init-db (&key clear)
  (when (or clear (null (probe-file *db-file*)))
    (v:info :db "Initialising database...")
    (when (probe-file *db-file*) (delete-file *db-file*))
    (let ((sql (create-table 'client
                             '(client_id :text)
                             '(last_refresh :integer)
                             '(access_token :text)
                             '(refresh_token :text)
                             '(expires_in :integer))))
      (v:debug :db "Executing '~a'" sql)
      (sqlite:with-open-database (db *db-file*)
        (sqlite:execute-non-query db sql)))))

(defun load-client-fields (client-id)
  (sqlite:with-open-database (db *db-file*)
    (multiple-value-bind (last-refresh access-token refresh-token expires-in)
        (values-list
         (first
          (sqlite:execute-to-list
           db
           "SELECT last_refresh, access_token, refresh_token, expires_in FROM client WHERE client_id = ?"
           client-id)))
      (when (and last-refresh access-token refresh-token expires-in)
        (list :last-refresh last-refresh
              :access-token access-token
              :refresh-token refresh-token
              :expires-in expires-in)))))

(defun store-client-fields (last-refresh access-token refersh-token expires-in client-id)
  (let ((query (if (load-client-fields client-id)
                   "UPDATE client SET last_refresh = ?, access_token = ?, refresh_token = ?, expires_in = ? WHERE client_id = ?"
                   "INSERT INTO client (last_refresh, access_token, refresh_token, expires_in, client_id) VALUES (?, ?, ?, ?, ?)")))
    (sqlite:with-open-database (db *db-file*)
      (sqlite:execute-non-query
       db query last-refresh access-token refersh-token expires-in client-id))))
