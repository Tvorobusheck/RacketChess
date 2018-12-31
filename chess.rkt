#lang racket

;;;; Шахматы на языке racket
;;;; Управление мышью
;;;;
;;;; В. В. Клименко, 2018


(require 2htdp/universe
         2htdp/image)

;;; Размеры игрового пространства.
(define sq-size 80)
(define width (* 8 sq-size))
(define height (* 8 sq-size))


;;; Состояние "мира".

;;;; Доска реализована в виде(кроме отрисовки пустой доски)
;;;; Y
;;;; 7 WBWBWBWB
;;;; 6 BWBWBWBW
;;;; 5 WBWBWBWB
;;;; 4 BWBWBWBW
;;;; 3 WBWBWBWB
;;;; 2 BWBWBWBW
;;;; 1 WBWBWBWB
;;;; 0 BWBWBWBW
;;;;   01234567 X

(struct world
  (x y        ; выбранная клетка
     selx sely ; выбранная фигура
     figures ; фигуры
     number-of-move) ; номер хода
  #:transparent)

;;;; Типы(type) фигур
;;;; 1 - пешка
;;;; 2 - конь
;;;; 3 - слон
;;;; 4 - ладья
;;;; 5 - ферзь
;;;; 6 - король

;;;; 0 - белый цвет
;;;; 1 - черный цвет
(struct figure
  (x y ; координаты
     color ; цвет
     type) ; тип
  #:transparent)
;;; Исходное состояние (ракета в центре в состоянии покоя).
(define figures0
  (append
   (map (lambda (pos) (figure pos 1 0 1)) (range 8))
   (map (lambda (pos) (figure pos 6 1 1)) (range 8))
   (list
    (figure 1 0 0 2)
    (figure 6 0 0 2)
    (figure 1 7 1 2)
    (figure 6 7 1 2))
   (list
    (figure 2 0 0 3)
    (figure 5 0 0 3)
    (figure 2 7 1 3)
    (figure 5 7 1 3))
   (list
    (figure 0 0 0 4)
    (figure 7 0 0 4)
    (figure 0 7 1 4)
    (figure 7 7 1 4))
   (list
    (figure 3 0 0 5)
    (figure 4 7 1 5))
   (list
    (figure 4 0 0 6)
    (figure 3 7 1 6))))
(define world0 (world -1 -1 -1 -1 figures0 0))
(define (find-figure figures x y col)
  (filter
   (lambda (cur)
     (and (= (figure-x cur) x)
          (= (figure-y cur) y)
          (= (figure-color cur) col)))
   figures))
(define (remove-figure figures x y)
  (filter
   (lambda (cur)
     (not (and (= (figure-x cur) x)
          (= (figure-y cur) y))))
   figures))
(define (add-figure fig figures)
  (cons fig figures))
(define (move-figure figures fig x y)
  (add-figure
   (figure x y (figure-color fig) (figure-type fig))
   (remove-figure
   (remove-figure figures (figure-x fig) (figure-y fig))
   x y)))
(define (find-figure-bytype figures type col)
  (filter
   (lambda (cur)
     (and (= (figure-type cur) type)
          (= (figure-color cur) col)))
   figures))
(define (isblack? x y)
  (even?  (+ x y)))
(define (signum num)
  (cond
    [(> num 0) 1]
    [(< num 0) -1]
    [(= num 0) 0]
    ))
(define (invert-color color)
  (if (= color 1)
      0
      1))
;; Возможно ли сходить фигурой в x y без учета шахов и матов
(define (placeble? figures fig x y)
  (cond
    [(not (empty? (find-figure
                   figures
                   x y
                   (figure-color fig)))) #f]
    ;; Пешка
    [(= (figure-type fig) 1)
     ;;Белые
     (if (even? (figure-color fig))
         (cond
           ;; Сходить пешкой прямо
           [(and (= y (add1 (figure-y fig))) (= x (figure-x fig)))
            (empty? (append
                     (find-figure figures x y 0)
                     (find-figure figures x y 1)))]
           ;; Сходить пешкой прямо на 2
           [(and (= 1 (figure-y fig))
                 (= y (+ 2 (figure-y fig)))
                 (= x (figure-x fig)))
            (empty? (append
                     (find-figure figures x y 0)
                     (find-figure figures x y 1)))]
           ;; Сходить пешкой наискосок
           [(and (= y (add1 (figure-y fig)))
                 (or (= x (add1 (figure-x fig)))
                     (= x (sub1 (figure-x fig)))))
            (not (empty? (find-figure figures x y 1)))]
           [else #f])
         (cond
           ;; Сходить пешкой прямо
           [(and (= y (sub1 (figure-y fig))) (= x (figure-x fig)))
            (empty? (append
                     (find-figure figures x y 0)
                     (find-figure figures x y 1)))]
           ;; Сходить пешкой прямо на 2
           [(and (= 6 (figure-y fig))
                 (= y (- (figure-y fig) 2))
                 (= x (figure-x fig)))
            (empty? (append
                     (find-figure figures x y 0)
                     (find-figure figures x y 1)))]
           ;; Сходить пешкой наискосок
           [(and (= y (sub1 (figure-y fig)))
                 (or (= x (add1 (figure-x fig)))
                     (= x (sub1 (figure-x fig)))))
            (not (empty? (find-figure figures x y 0)))]
           [else #f])
         )]
    ;;Конь
    [(= (figure-type fig) 2)
     (= 5 (+ (* (- x (figure-x fig)) (- x (figure-x fig)))
             (* (- y (figure-y fig)) (- y (figure-y fig)))))]
    ;;Слон
    [(and (= (figure-type fig) 3)
          (= (abs (- x (figure-x fig)))
             (abs (- y (figure-y fig)))))
       (if (= (abs (- x (figure-x fig))) 1)
           #t
           (if (empty? (find-figure
                   figures
                   (+ x (signum (- (figure-x fig) x)))
                   (+ y (signum (- (figure-y fig) y)))
                   (invert-color (figure-color fig))))
               (placeble? figures
                          fig
                          (+ x (signum (- (figure-x fig) x)))
                          (+ y (signum (- (figure-y fig) y))))
               #f))]
    ;;Ладья
    [(and (= (figure-type fig) 4)
          (or (= x (figure-x fig))
              (= y (figure-y fig))))
       (if (or (= (abs (- x (figure-x fig))) 1)
               (= (abs (- y (figure-y fig))) 1))
           #t
           (if (empty? (find-figure
                   figures
                   (+ x (signum (- (figure-x fig) x)))
                   (+ y (signum (- (figure-y fig) y)))
                   (invert-color (figure-color fig))))
               (placeble? figures
                          fig
                          (+ x (signum (- (figure-x fig) x)))
                          (+ y (signum (- (figure-y fig) y))))
               #f))]
    ;;Ферзь
    [(and (= (figure-type fig) 5)
          (or (= (abs (- x (figure-x fig)))
                 (abs (- y (figure-y fig))))
              (or (= x (figure-x fig))
                  (= y (figure-y fig)))))
       (if (or (= (abs (- x (figure-x fig))) 1)
               (= (abs (- y (figure-y fig))) 1))
           #t
           (if (empty? (find-figure
                   figures
                   (+ x (signum (- (figure-x fig) x)))
                   (+ y (signum (- (figure-y fig) y)))
                   (invert-color (figure-color fig))))
               (placeble? figures
                          fig
                          (+ x (signum (- (figure-x fig) x)))
                          (+ y (signum (- (figure-y fig) y))))
               #f))]
    ;;Король
    [(and (= (figure-type fig) 6)
          (<= (abs (- x (figure-x fig))) 1)
          (<= (abs (- y (figure-y fig))) 1))
       #t]
    [else #f]))
(define (check? figures color)
  (if (empty? (find-figure-bytype
               figures
               6
               color))
      #f
  (let ([king (car (find-figure-bytype
               figures
               6
               color))])
    (ormap
     (lambda (cur)
       (and (= (figure-color cur) (invert-color (figure-color king)))
            (placeble? figures cur (figure-x king) (figure-y king))))
     figures))))
(define (movable? figures fig x y color)
  (and (= color (figure-color fig))
       (not (check? (move-figure figures fig x y) color))
       (placeble? figures fig x y)))
(define (checkmate? figures color)
  (not (for*/first ([fig figures]
              [x (range 8)]
              [y (range 8)]
              #:when (movable? figures fig x y color))
    #t)))
(define (try-to-takemove ws x y)
  (cond [(movable?
       (world-figures ws)
       (car (find-figure
             (world-figures ws)
             (world-selx ws)
             (world-sely ws)
             (modulo (world-number-of-move ws) 2)))
       (pix->x x) (pix->y y)
       (modulo (world-number-of-move ws) 2))
      (world
       -1 -1
       -1 -1
       (move-figure
        (world-figures ws)
        (car (find-figure
              (world-figures ws)
              (world-selx ws)
              (world-sely ws)
              (modulo (world-number-of-move ws) 2)))
        (pix->x x)
        (pix->y y))
       (add1 (world-number-of-move ws)))]
      [else (world
       (pix->x x) (pix->y y)
       (if (empty? (find-figure
             (world-figures ws)
             (pix->x x)
             (pix->y y)
             (modulo (world-number-of-move ws) 2)))
           (world-selx ws)
           (pix->x x)) 
       (if (empty? (find-figure
             (world-figures ws)
             (pix->x x)
             (pix->y y)
             (modulo (world-number-of-move ws) 2)))
           (world-sely ws)
           (pix->y y))
       (world-figures ws)
       (world-number-of-move ws))]))
;; Обработчик мыши
(define (mouse-handler ws x y event)
  (if (and (string=? event "button-down")
           (not (checkmate?
                 (world-figures ws)
                 (modulo (world-number-of-move ws) 2))))
           (if (or (= (world-selx ws) -1)
              (= (world-sely ws) -1))
          (world (pix->x x) (pix->y y)
                 (if (empty? (find-figure
                              (world-figures ws)
                              (pix->x x)
                              (pix->y y)
                              (modulo (world-number-of-move ws) 2)))
                     (world-selx ws)
                     (figure-x (car (find-figure
                                     (world-figures ws)
                                     (pix->x x)
                                     (pix->y y)
                                     (modulo (world-number-of-move ws) 2)))))
                 (if (empty? (find-figure
                              (world-figures ws)
                              (pix->x x)
                              (pix->y y)
                              (modulo (world-number-of-move ws) 2)))
                     (world-sely ws)
                     (figure-y (car (find-figure
                                 (world-figures ws)
                                 (pix->x x)
                                 (pix->y y)
                                 (modulo (world-number-of-move ws) 2)))))
                 (world-figures ws)
                 (world-number-of-move ws))
          (try-to-takemove ws x y))
          ws
          ))
(define (dsq x y)
  (rectangle
   sq-size
   sq-size
   "solid"
   (if (isblack? x y) "brown" "gray")))
(define (drf base)
  (foldr (lambda (pos prev)
           (above prev (dsq (car pos) (cdr pos))))
         empty-image
         base))

(define (drs base)
  (foldr (lambda (cur res) (beside cur res))
         empty-image
         base))
(define (draw-x y) (drf (map (lambda (x) (cons y x)) (range 8))))
(define (draw-empty-board k)
  (drs (map draw-x (range 8))))

(define (pix->x x)
  (quotient x sq-size))
(define (pix->y y)
  (quotient (- height y) sq-size))
(define (x->pix x)
  (+ (/ sq-size 2) (* sq-size x)))
(define (y->pix y)
  (- height (+ (/ sq-size 2) (* sq-size y))))
(define (draw-figure fig)
  (if (zero? (figure-color fig))
      (cond
        [(= (figure-type fig) 1)
         (bitmap/file "images/white/Chess_plt60.png")]
        [(= (figure-type fig) 2)
         (bitmap/file "images/white/Chess_nlt60.png")]
        [(= (figure-type fig) 3)
         (bitmap/file "images/white/Chess_blt60.png")]
        [(= (figure-type fig) 4)
         (bitmap/file "images/white/Chess_rlt60.png")]
        [(= (figure-type fig) 5)
         (bitmap/file "images/white/Chess_qlt60.png")]
        [(= (figure-type fig) 6)
         (bitmap/file "images/white/Chess_klt60.png")]
        [else (rectangle sq-size sq-size 'solid 'pink)]
        )
      (cond
        [(= (figure-type fig) 1)
         (bitmap/file "images/black/Chess_pdt60.png")]
        [(= (figure-type fig) 2)
         (bitmap/file "images/black/Chess_ndt60.png")]
        [(= (figure-type fig) 3)
         (bitmap/file "images/black/Chess_bdt60.png")]
        [(= (figure-type fig) 4)
         (bitmap/file "images/black/Chess_rdt60.png")]
        [(= (figure-type fig) 5)
         (bitmap/file "images/black/Chess_qdt60.png")]
        [(= (figure-type fig) 6)
         (bitmap/file "images/black/Chess_kdt60.png")]
        [else (rectangle sq-size sq-size 'solid 'red)])))
    
(define (draw-board w)
  (foldr
   (lambda
       (cur prev)
     (place-image
      (scale (/ sq-size (image-width (draw-figure cur))) (draw-figure cur))
      (x->pix (figure-x cur)) (y->pix (figure-y cur))
      prev))
   (draw-empty-board 8)
   (world-figures w)))
(define (win-message color)
   (rectangle width height 'solid 'green))
(define (draw w)
  (if (checkmate? (world-figures w)
                  (modulo (world-number-of-move w) 2))
      (win-message (invert-color
                    (modulo (world-number-of-move w) 2)))
      (let ([board 
         (if
          (or (negative? (world-x w))
              (negative? (world-y w)))
          (draw-board w)
          (place-image
         ;; Выбранная клетка
         (rectangle sq-size sq-size 'outline 'green)
         (x->pix (world-x w))
         (y->pix (world-y w))
         ;; Доска
         (draw-board w)))])
    (if (or (negative?
             (world-selx w))
            (negative?
             (world-sely w)))
        board
        (place-image
         (rectangle sq-size sq-size 'outline 'red)
         (x->pix (world-selx w)) (y->pix (world-sely w))
         board)))))
(define (start)
  (big-bang world0
            (on-mouse mouse-handler)
           (on-draw draw)))
