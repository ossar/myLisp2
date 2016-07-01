(load "dice_of_doom_v1.lisp")
(load "lazy.lisp")

; ボードサイズの再定義
(defparameter *board-size* 4)
(defparameter *board-hexnum* (* *board-size* *board-size*))

; 相手に手を渡す（遅延バージョン）
(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
    moves
    (lazy-cons (list nil
                     (game-tree (add-new-dice board player (1- spare-dice))
                                (mod (1+ player) *num-players*)
                                0
                                t))
               moves)))

; 攻撃する手を追加する（遅延バージョン）
(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
                   (car (aref board pos)))
           (dice (pos)
                 (cadr (aref board pos))))
    (lazy-mapcan (lambda (src)
                   (if (eq (player src) cur-player)
                     (lazy-mapcan (lambda (dst)
                                    (if (and (not (eq (player dst) cur-player))
                                             (> (dice src) (dice dst)))
                                      (make-lazy
                                        (list (list (list src dst)
                                                    (game-tree (board-attack board
                                                                             cur-player
                                                                             src
                                                                             dst
                                                                             (dice src))
                                                               cur-player
                                                               (+ spare-dice (dice dst))
                                                               nil))))
                                      (lazy-nil)))
                                  (make-lazy (neighbors src)))
                     (lazy-nil)))
                 (make-lazy (loop for n below *board-hexnum*
                                  collect n)))))

; 人間の入力を行う（遅延バージョン）
(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (caddr tree)))
    (labels ((print-moves (moves n)
                          (unless (lazy-null moves)
                            (let* ((move (lazy-car moves))
                                   (action (car move)))
                              (fresh-line)
                              (format t "~a. " n)
                              (if action
                                (format t "~a -> ~a" (car action) (cadr action))
                                (princ "end turn")))
                            (print-moves (lazy-cdr moves) (1+ n)))))
      (print-moves moves 1))
    (fresh-line)
    (cadr (lazy-nth (1- (read)) moves))))

; 人間同士の戦い（遅延バージョン）
(defun play-vs-human (tree)
  (print-info tree)
  (if (not (lazy-null (caddr tree)))
    (play-vs-human (handle-human tree))
    (announce-winner (cadr tree))))

; 探索木の刈り込み
(defun limit-tree-depth (tree depth)
  (list (car tree)
        (cadr tree)
        (if (zerop depth)
          (lazy-nil)
          (lazy-mapcar (lambda (move)
                         (list (car move)
                               (limit-tree-depth (cadr move) (1- depth))))
                       (caddr tree)))))

; 探索レベルの指定
(defparameter *ai-level* 4)

; コンピューター入力する（遅延バージョン）
(defun handle-computer (tree)
  (let ((ratings (get-ratings (limit-tree-depth tree *ai-level*)
                              (car tree))))
    (cadr (lazy-nth (position (apply #'max ratings) ratings)
                    (caddr tree)))))

; コンピューター対戦する（遅延バージョン）
(defun play-vs-computer (tree)
  (print-info tree)
  (cond ((lazy-null (caddr tree)) (announce-winner (cadr tree)))
        ((zerop (car tree)) (play-vs-computer (handle-human tree)))
        (t (play-vs-computer (handle-computer tree)))))

; 盤評価
(defun score-board (board player)
  (loop for hex across board
        for pos from 0
        sum (if (eq (car hex) player)
              (if (threatened pos board)
                1
                2)
              -1)))

; 脅かされているか判定
(defun threatened (pos board)
  (let* ((hex (aref board pos))
         (player (car hex))
         (dice (cadr hex)))
    (loop for n in (neighbors pos)
          do (let* ((nhex (aref board n))
                    (nplayer (car nhex))
                    (ndice (cadr nhex)))
               (when (and (not (eq player nplayer)) (> ndice dice))
                 (return t))))))

; レーティングを得る（遅延バージョン）
(defun get-ratings (tree player)
  (take-all (lazy-mapcar (lambda (move)
                           (rate-position (cadr move) player))
                         (caddr tree))))

; レーティングを計算する（遅延バージョン）
(defun rate-position (tree player)
  (let ((moves (caddr tree)))
    (if (not (lazy-null moves))
      (apply (if (eq (car tree) player)
               #'max
               #'min)
             (get-ratings tree player))
      (score-board (cadr tree) player))))

