#lang racket

;;;; Шахматы на языке racket
;;;; Управление мышью
;;;;
;;;; В. В. Клименко, 2018


(require 2htdp/universe
         2htdp/image)

;;; Размеры игрового пространства.
(define sq-size 60)

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
     caslfigs ; фигуры которые могут рокироваться
     prev-figures ; прошлые расстановки фигур
     prev-caslfigs) ; прошлые фигуры для рокировки
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

;; Расстановка для тестов
(define figuresTEST  
 (list
  (figure 0 0 0 4)
  (figure 7 0 0 4)
  (figure 0 7 1 4)
  (figure 7 7 1 4)
  (figure 4 0 0 6)
  (figure 3 7 1 6)
  (figure 2 1 0 1)
  (figure 5 6 1 1)))

(define caslfigs0
  (append
   (list
    (figure 0 0 0 4)
    (figure 7 0 0 4)
    (figure 0 7 1 4)
    (figure 7 7 1 4))
   (list
    (figure 4 0 0 6)
    (figure 3 7 1 6))))

(define world0
  (world -1 -1 -1 -1 figures0 caslfigs0 '() '()))

(define (find-figure figures x y col)
  (filter
   (lambda (cur)
     (and (= (figure-x cur) x)
          (= (figure-y cur) y)
          (or (= (figure-color cur) col)
              (= col 3))))
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

(define (get-cur-col ws)
  (modulo (length (world-prev-figures ws)) 2))

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
                     (find-figure figures x (sub1 y) 3)
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
                     (find-figure figures x (add1 y) 3)
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

(define (pat? figures color)
  (not (for*/first ([fig
                     (filter (lambda (fig) (= (figure-color fig) color))
                               figures)]
              [x (range 8)]
              [y (range 8)]
              #:when (movable? figures fig x y color))
         (list fig x y color))))

(define (checkmate? figures color)
  (and (check? figures color)
       (pat? figures color)))

;; Пешка дошла до противоположного края доскиx
(define (enpassant? figures color)
  (not (empty? (filter
                (lambda (cur)
                  (and (= (figure-color cur) color)
                       (= (figure-type cur) 1)
                       (or (and
                            (= 0 (figure-color cur))
                            (= 7 (figure-y cur)))
                           (and
                            (= 1 (figure-color cur))
                            (= 0 (figure-y cur))))))
                figures))))

;; Превратить крайнюю пешку в ферзя
(define (turn-enpassant figures color type)
  (let ([pawn (car (filter
                (lambda (cur)
                  (and (= (figure-color cur) color)
                       (= (figure-type cur) 1)
                       (or (and
                            (= 0 (figure-color cur))
                            (= 7 (figure-y cur)))
                           (and
                            (= 1 (figure-color cur))
                            (= 0 (figure-y cur))))))
                figures))])
    (add-figure
     (figure (figure-x pawn)
             (figure-y pawn)
             (figure-color pawn)
             type)
     (remove-figure figures (figure-x pawn) (figure-y pawn)))))

;; Рокировка без учета шахов
(define (castling? figures caslfigs fig x y)
  (and (= (figure-type fig) 6)
       (not (empty? (find-figure
                     caslfigs
                     (figure-x fig)
                     (figure-y fig)
                     (figure-color fig))))
       (= (figure-y fig) y)
       (= (abs (- (figure-x fig) x)) 2)
       (if (= (figure-color fig) 0)
           (and (= y 0)
                (or
                 ;; белые
                 ;; рокировка вправо
                 (and (> x 4)
                      (let ([rook (find-figure
                                   caslfigs
                                   7 y
                                   (figure-color fig))])
                        (and (not (empty? rook))
                             (= (figure-type (car rook)) 4)
                             (empty? (append
                                      (find-figure
                                       figures
                                       6 y
                                       3)
                                      (find-figure
                                  figures
                                  5 y
                                  3))))))
                 ;; рокировка влево
                 (and (< x 4)
                      (let ([rook (find-figure
                                   caslfigs
                                   0 y
                                   (figure-color fig))])
                        (and (not (empty? rook))
                             (= (figure-type (car rook)) 4)
                             (empty? (append
                                      (find-figure
                                       figures
                                       3 y
                                       3)
                                      (find-figure
                                       figures
                                       2 y
                                       3)
                                      (find-figure
                                       figures
                                       1 y
                                       3))))))))
           (and (= y 7)
                (or
                 ;; черные
                 ;; рокировка вправо
                 (and (> x 3)
                      (let ([rook (find-figure
                                   caslfigs
                                   7 y
                                   (figure-color fig))])
                        (and (not (empty? rook))
                             (= (figure-type (car rook)) 4)
                             (empty? (append
                                      (find-figure
                                       figures
                                       6 y
                                       3)
                                 (find-figure
                                  figures
                                  5 y
                                  3)
                                 (find-figure
                                  figures
                                  4 y
                                  3))))))
                 ;; рокировка влево
                 (and (< x 4)
                      (let ([rook (find-figure
                                   caslfigs
                                   0 y
                                   (figure-color fig))])
                        (and (not (empty? rook))
                             (= (figure-type (car rook)) 4)
                             (empty? (append
                                      (find-figure
                                       figures
                                       2 y
                                       3)
                                      (find-figure
                                       figures
                                       1 y
                                       3)))))))))))

(define (castling-possible? figures caslfigs fig x y color)
  (and (castling? figures caslfigs fig x y)
       (not (check? figures color))
       ;; Поле которое минует король тоже не должно биться
       (not (check? (move-figure
                     figures
                     fig
                     (+ (figure-x fig) (signum (- x (figure-x fig))))
                     y)
                    color))
       (not (check? (move-figure figures fig x y) color))))

(define (take-castling figures king x y)
  (if (zero? (figure-color king))
      ;; Белые
      (if (> x 4)
          ;; Вправо
          (move-figure
           (move-figure
            figures
            king
            x y)
           (car (find-figure
                 figures
                 7 0
                 0))
           5 0)
          ;; Влево
          (move-figure
           (move-figure
            figures
            king
            x y)
           (car (find-figure
                 figures
                 0 0
                 0))
           3 0))
      ;; Черные
      (if (> x 3)
          ;; Вправо
          (move-figure
           (move-figure
            figures
            king
            x y)
           (car (find-figure
                 figures
                 7 7
                 1))
           4 7)
          ;; Влево
          (move-figure
           (move-figure
            figures
            king
            x y)
           (car (find-figure
                 figures
                 0 7
                 1))
           2 7))))

;; Обработчик ходов
(define (try-to-takemove ws x y)
  ;; Пешка идет до края
  (cond [(and (not (enpassant? 
               (world-figures ws)
               (get-cur-col ws)))
              (movable?
               (world-figures ws)
               (car (find-figure
                     (world-figures ws)
                     (world-selx ws)
                     (world-sely ws)
                     (get-cur-col ws)))
               (pix->x x) (pix->y y)
               (get-cur-col ws))
              (enpassant? 
               (move-figure
                (world-figures ws)
                (car (find-figure
                      (world-figures ws)
                      (world-selx ws)
                      (world-sely ws)
                      (get-cur-col ws)))
                (pix->x x)
                (pix->y y))
               (get-cur-col ws)))
         (world
          -1 -1
          -1 -1
          (move-figure
           (world-figures ws)
           (car (find-figure
                 (world-figures ws)
                 (world-selx ws)
                 (world-sely ws)
                 (get-cur-col ws)))
           (pix->x x)
           (pix->y y))
          (remove-figure
           (remove-figure (world-caslfigs ws) 
                          (figure-x
                           (car (find-figure
                                 (world-figures ws)
                                 (world-selx ws)
                                 (world-sely ws)
                                 (get-cur-col ws)))) 
                          (figure-y
                           (car (find-figure
                                 (world-figures ws)
                                 (world-selx ws)
                                 (world-sely ws)
                                 (get-cur-col ws)))))
           x y)
          (cons (world-figures ws)
                (cons (world-figures ws) (world-prev-figures ws)))
          (cons (world-caslfigs ws)
                (cons (world-caslfigs ws) (world-prev-caslfigs ws))))]
        ;; Обычный ходx
        [(and (not (enpassant? 
                    (world-figures ws)
                    (get-cur-col ws)))
              (movable?
               (world-figures ws)
               (car (find-figure
                     (world-figures ws)
                     (world-selx ws)
                     (world-sely ws)
                     (get-cur-col ws)))
               (pix->x x) (pix->y y)
               (get-cur-col ws)))
         (world
          -1 -1
          -1 -1
          (move-figure
           (world-figures ws)
           (car (find-figure
                 (world-figures ws)
                 (world-selx ws)
                 (world-sely ws)
                 (get-cur-col ws)))
           (pix->x x)
           (pix->y y))
          (remove-figure
           (remove-figure (world-caslfigs ws) 
                          (figure-x
                           (car (find-figure
                                 (world-figures ws)
                                 (world-selx ws)
                                 (world-sely ws)
                                 (get-cur-col ws)))) 
                          (figure-y
                           (car (find-figure
                                 (world-figures ws)
                                 (world-selx ws)
                                 (world-sely ws)
                                 (get-cur-col ws)))))
           x y)
          (cons (world-figures ws) (world-prev-figures ws))
          (cons (world-caslfigs ws) (world-prev-caslfigs ws)))]
        [(and (not (enpassant? 
                    (world-figures ws)
                    (get-cur-col ws)))
              (castling-possible?
               (world-figures ws)
               (world-caslfigs ws)
               (car (find-figure
                     (world-figures ws)
                     (world-selx ws)
                     (world-sely ws)
                     (get-cur-col ws)))
               (pix->x x) (pix->y y)
               (get-cur-col ws)))
         (world
          -1 -1
          -1 -1
          (take-castling
           (world-figures ws)
           (car (find-figure
                (world-figures ws)
                (world-selx ws)
                (world-sely ws)
                (get-cur-col ws)))
           (pix->x x) (pix->y y))
           (remove-figure (world-caslfigs ws) 
                          (figure-x
                          (car (find-figure
                                (world-figures ws)
                                (world-selx ws)
                                (world-sely ws)
                                (get-cur-col ws))))
                          (figure-y
                          (car (find-figure
                                (world-figures ws)
                                (world-selx ws)
                                (world-sely ws)
                                (get-cur-col ws)))))
           (cons (world-figures ws)
                 (world-prev-figures ws))
           (cons (world-figures ws)
                 (world-prev-caslfigs ws)))]
        [else (world
               (pix->x x) (pix->y y)
               (if (empty? (find-figure
                            (world-figures ws)
                            (pix->x x)
                            (pix->y y)
                            (get-cur-col ws)))
                   (world-selx ws)
                   (pix->x x)) 
               (if (empty? (find-figure
                            (world-figures ws)
                            (pix->x x)
                            (pix->y y)
                            (get-cur-col ws)))
                   (world-sely ws)
                   (pix->y y))
               (world-figures ws)
               (world-caslfigs ws)
               (world-prev-figures ws)
               (world-prev-caslfigs ws))]))

;; Обработчик мыши
(define (mouse-handler ws x y event)
  (if (string=? event "button-down")
      (if (enpassant? (world-figures ws)
                      (get-cur-col ws))
          (let ([figures (cond
                           [(<= (+ (/ height 2) sq-size)
                                y
                                (+ (/ height 2) (* 2 sq-size)))
                            (turn-enpassant (world-figures ws)
                                            (get-cur-col ws)
                                            5)]
                           [(<= (/ height 2)
                                y
                                (+ (/ height 2) sq-size))
                            (turn-enpassant (world-figures ws)
                                            (get-cur-col ws)
                                            4)]
                           [(>= (/ height 2)
                                y
                                (- (/ height 2) sq-size))
                            (turn-enpassant (world-figures ws)
                                            (get-cur-col ws)
                                            3)]
                           [(>= (- (/ height 2) sq-size)
                                y
                                (- (/ height 2) (* 2 sq-size)))
                            (turn-enpassant (world-figures ws)
                                            (get-cur-col ws)
                                            2)]
                           [else empty])])
            (if (empty? figures)
                ws
                (world
                 -1 -1
                 -1 -1
                 figures
                 (remove-figure (world-caslfigs ws) 
                                (pix->x x) (pix->y y))
                 (cdr (world-prev-figures ws))
                 (cdr (world-prev-caslfigs ws)))))
            (if (or (= (world-selx ws) -1)
                    (= (world-sely ws) -1))
                (world (pix->x x) (pix->y y)
                       (if (empty? (find-figure
                                    (world-figures ws)
                                    (pix->x x)
                                    (pix->y y)
                                    (get-cur-col ws)))
                           (world-selx ws)
                           (figure-x (car (find-figure
                                           (world-figures ws)
                                           (pix->x x)
                                           (pix->y y)
                                           (get-cur-col ws)))))
                       (if (empty? (find-figure
                                    (world-figures ws)
                                    (pix->x x)
                                    (pix->y y)
                                    (get-cur-col ws)))
                           (world-sely ws)
                           (figure-y (car (find-figure
                                           (world-figures ws)
                                           (pix->x x)
                                           (pix->y y)
                                           (get-cur-col ws)))))
                       (world-figures ws)
                       (world-caslfigs ws)
                       (world-prev-figures ws)
                       (world-prev-caslfigs ws))
                (try-to-takemove ws x y)))
          ws
          ))

;; Обработчик клавиш
(define (key-handler w k)
  (if (and (key=? k "\b")
           (not (enpassant? (world-figures w) (get-cur-col w)))
           (not (empty? (world-prev-figures w))))
      (world
       -1 -1
       -1 -1
       (car (world-prev-figures w))
       (car (world-prev-caslfigs w))
       (cdr (world-prev-figures w))
       (cdr (world-prev-caslfigs w)))
      w))

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

(define (draw-x y)
  (drf (map (lambda (x) (cons y x)) (range 8))))

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

(define (draw-game w)
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
         board))))

(define (draw-message txt)
  (place-image
   (text txt sq-size 'black)
  (/ width 2) (/ sq-size 2)
  (rectangle width sq-size 'solid 'white)))

(define (draw ws)
  (place-image
   (cond
     [(checkmate?
       (world-figures ws)
       (get-cur-col ws))
      (draw-message
       (if (zero? (get-cur-col ws))
           "Black win!"
           "White win!"))]
     [(pat?
       (world-figures ws)
       (get-cur-col ws))
      (draw-message "Draw")]
     [(enpassant?
       (world-figures ws)
       (get-cur-col ws))
      (above (draw-message "Knight")
             (draw-message "Bishop")
             (draw-message "Rook")
             (draw-message "Queen"))]
     [else empty-image])
  (/ width 2) (/ height 2)
  (draw-game ws)))

(define (start)
  (big-bang world0
            (on-mouse mouse-handler)
            (on-key key-handler)
            (on-draw draw)))
(start)
