#lang racket

;;;; Шахматы на языке racket
;;;; Управление мышью
;;;; Backspace для отмены хода
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
     caslfigs ; фигуры которые могут рокироваться
     prev-figures ; прошлые расстановки фигур
     prev-caslfigs) ; прошлые фигуры для рокировки
  #:transparent)

;;;; Типы(type) фигур
;;;; pawn - пешка
;;;; knight - конь
;;;; bishop - слон
;;;; rook - ладья
;;;; queen - ферзь
;;;; king - король

;;;; white - белый цвет
;;;; black - черный цвет
(struct figure
  (x y ; координаты
     color ; цвет
     type) ; тип
  #:transparent)

;;; Исходное состояние (ракета в центре в состоянии покоя).
(define figures0
  (append
   (map (lambda (pos) (figure pos 1 'white 'pawn)) (range 8))
   (map (lambda (pos) (figure pos 6 'black 'pawn)) (range 8))
   (list
    (figure 1 0 'white 'knight)
    (figure 6 0 'white 'knight)
    (figure 1 7 'black 'knight)
    (figure 6 7 'black 'knight))
   (list
    (figure 2 0 'white 'bishop)
    (figure 5 0 'white 'bishop)
    (figure 2 7 'black 'bishop)
    (figure 5 7 'black 'bishop))
   (list
    (figure 0 0 'white 'rook)
    (figure 7 0 'white 'rook)
    (figure 0 7 'black 'rook)
    (figure 7 7 'black 'rook))
   (list
    (figure 3 0 'white 'queen)
    (figure 4 7 'black 'queen))
   (list
    (figure 4 0 'white 'king)
    (figure 3 7 'black 'king))))

;; Расстановка для тестов
(define figuresTEST  
 (list
  (figure 0 0 'white 'rook)
  (figure 7 0 'white 'rook)
  (figure 0 7 'black 'rook)
  (figure 7 7 'black 'rook)
  (figure 4 0 'white 'king)
  (figure 3 7 'black 'king)
  (figure 2 1 'white 'pawn)
  (figure 5 6 'black 'pawn)))

(define caslfigs0
  (append
   (list
    (figure 0 0 'white 'rook)
    (figure 7 0 'white 'rook)
    (figure 0 7 'black 'rook)
    (figure 7 7 'black 'rook))
   (list
    (figure 4 0 'white 'king)
    (figure 3 7 'black 'king))))

(define world0
  (world -1 -1 -1 -1 figures0 caslfigs0 '() '()))

(define (find-figure figures x y col)
  (for/first
      ([cur figures]
       #:when (and (= (figure-x cur) x)
                   (= (figure-y cur) y)
                   (or (eq? (figure-color cur) col)
                       (eq? col 'any))))
    cur))

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
  (for/first ([cur figures]
     #:when (and (eq? (figure-type cur) type)
          (eq? (figure-color cur) col)))
    cur))

(define (isblack? x y)
  (even?  (+ x y)))

(define (signum num)
  (cond
    [(> num 0) 1]
    [(< num 0) -1]
    [(= num 0) 0]
    ))

(define (invert-color color)
  (if (eq? color 'white)
      'black
      'white))

(define (get-cur-col ws)
  (if (zero? (modulo (length (world-prev-figures ws)) 2))
      'white
      'black))

(define (white? col)
  (eq? col 'white))
;; Возможно ли сходить фигурой в x y без учета шахов и матов
(define (placeble? figures fig x y)
  (cond
    [(find-figure
      figures
      x y
      (figure-color fig))
     #f]
    ;; Пешка
    [(eq? (figure-type fig) 'pawn)
     ;;Белые
     (if (white? (figure-color fig))
         (cond
           ;; Сходить пешкой прямо
           [(and (= y (add1 (figure-y fig))) (= x (figure-x fig)))
            (not (find-figure figures x y 'any))]
           ;; Сходить пешкой прямо на 2
           [(and (= 1 (figure-y fig))
                 (= y (+ 2 (figure-y fig)))
                 (= x (figure-x fig)))
            (not (or
                  (find-figure figures x (sub1 y) 'any)
                  (find-figure figures x y 'any)))]
           ;; Сходить пешкой наискосок
           [(and (= y (add1 (figure-y fig)))
                 (or (= x (add1 (figure-x fig)))
                     (= x (sub1 (figure-x fig)))))
            (find-figure figures x y 'black)]
           [else #f])
         (cond
           ;; Сходить пешкой прямо
           [(and (= y (sub1 (figure-y fig))) (= x (figure-x fig)))
            (not (find-figure figures x y 'any))]
           ;; Сходить пешкой прямо на 2
           [(and (= 6 (figure-y fig))
                 (= y (- (figure-y fig) 2))
                 (= x (figure-x fig)))
            (not (or
                     (find-figure figures x (add1 y) 'any)
                     (find-figure figures x y 'any)))]
           ;; Сходить пешкой наискосок
           [(and (= y (sub1 (figure-y fig)))
                 (or (= x (add1 (figure-x fig)))
                     (= x (sub1 (figure-x fig)))))
            (find-figure figures x y 'white)]
           [else #f])
         )]
    ;;Конь
    [(eq? (figure-type fig) 'knight)
     (= 5 (+ (* (- x (figure-x fig)) (- x (figure-x fig)))
             (* (- y (figure-y fig)) (- y (figure-y fig)))))]
    ;;Слон
    [(and (eq? (figure-type fig) 'bishop)
          (= (abs (- x (figure-x fig)))
             (abs (- y (figure-y fig)))))
       (if (= (abs (- x (figure-x fig))) 1)
           #t
           (if (not (find-figure
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
    [(and (eq? (figure-type fig) 'rook)
          (or (= x (figure-x fig))
              (= y (figure-y fig))))
       (if (or (= (abs (- x (figure-x fig))) 1)
               (= (abs (- y (figure-y fig))) 1))
           #t
           (if (not (find-figure
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
    [(and (eq? (figure-type fig) 'queen)
          (or (= (abs (- x (figure-x fig)))
                 (abs (- y (figure-y fig))))
              (or (= x (figure-x fig))
                  (= y (figure-y fig)))))
       (if (or (= (abs (- x (figure-x fig))) 1)
               (= (abs (- y (figure-y fig))) 1))
           #t
           (if (not (find-figure
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
    [(and (eq? (figure-type fig) 'king)
          (<= (abs (- x (figure-x fig))) 1)
          (<= (abs (- y (figure-y fig))) 1))
       #t]
    [else #f]))

(define (check? figures color)
  (if (not (find-figure-bytype
               figures
               'king
               color))
      #f
  (let ([king (find-figure-bytype
               figures
               'king
               color)])
    (ormap
     (lambda (cur)
       (and (eq? (figure-color cur) (invert-color (figure-color king)))
            (placeble? figures cur (figure-x king) (figure-y king))))
     figures))))

(define (movable? figures fig x y color)
  (and (eq? color (figure-color fig))
       (not (check? (move-figure figures fig x y) color))
       (placeble? figures fig x y)))

(define (pat? figures color)
  (not (for*/first ([fig
                     (filter (lambda (fig)
                               (eq? (figure-color fig) color))
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
                  (and (eq? (figure-color cur) color)
                       (eq? (figure-type cur) 'pawn)
                       (or (and
                            (eq? 'white (figure-color cur))
                            (eq? 7 (figure-y cur)))
                           (and
                            (eq? 'black (figure-color cur))
                            (eq? 0 (figure-y cur))))))
                figures))))

;; Превратить крайнюю пешку в ферзя
(define (turn-enpassant figures color type)
  (let ([pawn (car (filter
                (lambda (cur)
                  (and (eq? (figure-color cur) color)
                       (eq? (figure-type cur) 'pawn)
                       (or (and
                            (eq? 'white (figure-color cur))
                            (eq? 7 (figure-y cur)))
                           (and
                            (eq? 'black (figure-color cur))
                            (eq? 0 (figure-y cur))))))
                figures))])
    (add-figure
     (figure (figure-x pawn)
             (figure-y pawn)
             (figure-color pawn)
             type)
     (remove-figure figures (figure-x pawn) (figure-y pawn)))))

;; Рокировка без учета шахов
(define (castling? figures caslfigs fig x y)
  (and (eq? (figure-type fig) 'king)
       (find-figure
        caslfigs
        (figure-x fig)
        (figure-y fig)
        (figure-color fig))
       (= (figure-y fig) y)
       (= (abs (- (figure-x fig) x)) 2)
       (if (eq? (figure-color fig) 'white)
           (and (= y 0)
                (or
                 ;; белые
                 ;; рокировка вправо
                 (and (> x 4)
                      (let ([rook (find-figure
                                   caslfigs
                                   7 y
                                   (figure-color fig))])
                        (and rook
                             (eq? (figure-type rook) 'rook)
                             (not (or
                                   (find-figure
                                    figures
                                    6 y
                                    3)
                                   (find-figure
                                    figures
                                    5 y
                                    'any))))))
                 ;; рокировка влево
                 (and (< x 4)
                      (let ([rook (find-figure
                                   caslfigs
                                   0 y
                                   (figure-color fig))])
                        (and rook
                             (eq? (figure-type rook) 'rook)
                             (not (or
                                   (find-figure
                                    figures
                                    3 y
                                    'any)
                                   (find-figure
                                    figures
                                    2 y
                                    'any)
                                   (find-figure
                                    figures
                                    1 y
                                    'any))))))))
           (and (= y 7)
                (or
                 ;; черные
                 ;; рокировка вправо
                 (and (> x 3)
                      (let ([rook (find-figure
                                   caslfigs
                                   7 y
                                   (figure-color fig))])
                        (and rook
                             (eq? (figure-type rook) 'rook)
                             (not (or
                                   (find-figure
                                    figures
                                    6 y
                                    'any)
                                   (find-figure
                                    figures
                                    5 y
                                    'any)
                                   (find-figure
                                    figures
                                    4 y
                                    'any))))))
                 ;; рокировка влево
                 (and (< x 4)
                      (let ([rook (find-figure
                                   caslfigs
                                   0 y
                                   (figure-color fig))])
                        (and rook
                             (eq? (figure-type rook) 'rook)
                             (not (or
                              (find-figure
                               figures
                               2 y
                               'any)
                              (find-figure
                               figures
                               1 y
                               'any)))))))))))

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
  (if (white? (figure-color king))
      ;; Белые
      (if (> x 4)
          ;; Вправо
          (move-figure
           (move-figure
            figures
            king
            x y)
           (find-figure
                 figures
                 7 0
                 'white)
           5 0)
          ;; Влево
          (move-figure
           (move-figure
            figures
            king
            x y)
           (find-figure
            figures
            0 0
            'white)
           3 0))
      ;; Черные
      (if (> x 3)
          ;; Вправо
          (move-figure
           (move-figure
            figures
            king
            x y)
           (find-figure
            figures
            7 7
            'black)
           4 7)
          ;; Влево
          (move-figure
           (move-figure
            figures
            king
            x y)
           (find-figure
            figures
            0 7
            'black)
           2 7))))

;; Обработчик ходов
(define (try-to-takemove ws x y)
  ;; Пешка идет до края
  (cond [(and (not (enpassant? 
               (world-figures ws)
               (get-cur-col ws)))
              (movable?
               (world-figures ws)
               (find-figure
                (world-figures ws)
                (world-selx ws)
                (world-sely ws)
                (get-cur-col ws))
               (pix->x x) (pix->y y)
               (get-cur-col ws))
              (enpassant? 
               (move-figure
                (world-figures ws)
                (find-figure
                 (world-figures ws)
                 (world-selx ws)
                 (world-sely ws)
                 (get-cur-col ws))
                (pix->x x)
                (pix->y y))
               (get-cur-col ws)))
         (world
          -1 -1
          -1 -1
          (move-figure
           (world-figures ws)
           (find-figure
            (world-figures ws)
            (world-selx ws)
            (world-sely ws)
            (get-cur-col ws))
           (pix->x x)
           (pix->y y))
          (remove-figure
           (remove-figure (world-caslfigs ws) 
                          (figure-x
                           (find-figure
                            (world-figures ws)
                            (world-selx ws)
                            (world-sely ws)
                            (get-cur-col ws))) 
                          (figure-y
                           (find-figure
                            (world-figures ws)
                            (world-selx ws)
                            (world-sely ws)
                            (get-cur-col ws))))
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
               (find-figure
                (world-figures ws)
                (world-selx ws)
                (world-sely ws)
                (get-cur-col ws))
               (pix->x x) (pix->y y)
               (get-cur-col ws)))
         (world
          -1 -1
          -1 -1
          (move-figure
           (world-figures ws)
           (find-figure
            (world-figures ws)
            (world-selx ws)
            (world-sely ws)
            (get-cur-col ws))
           (pix->x x)
           (pix->y y))
          (remove-figure
           (remove-figure (world-caslfigs ws) 
                          (figure-x
                           (find-figure
                            (world-figures ws)
                            (world-selx ws)
                            (world-sely ws)
                            (get-cur-col ws))) 
                          (figure-y
                           (find-figure
                            (world-figures ws)
                            (world-selx ws)
                            (world-sely ws)
                            (get-cur-col ws))))
           x y)
          (cons (world-figures ws) (world-prev-figures ws))
          (cons (world-caslfigs ws) (world-prev-caslfigs ws)))]
        [(and (not (enpassant? 
                    (world-figures ws)
                    (get-cur-col ws)))
              (castling-possible?
               (world-figures ws)
               (world-caslfigs ws)
               (find-figure
                (world-figures ws)
                (world-selx ws)
                (world-sely ws)
                (get-cur-col ws))
               (pix->x x) (pix->y y)
               (get-cur-col ws)))
         (world
          -1 -1
          -1 -1
          (take-castling
           (world-figures ws)
           (find-figure
            (world-figures ws)
            (world-selx ws)
            (world-sely ws)
            (get-cur-col ws))
           (pix->x x) (pix->y y))
           (remove-figure (world-caslfigs ws) 
                          (figure-x
                          (find-figure
                           (world-figures ws)
                           (world-selx ws)
                           (world-sely ws)
                           (get-cur-col ws)))
                          (figure-y
                          (find-figure
                           (world-figures ws)
                           (world-selx ws)
                           (world-sely ws)
                           (get-cur-col ws))))
           (cons (world-figures ws)
                 (world-prev-figures ws))
           (cons (world-figures ws)
                 (world-prev-caslfigs ws)))]
        [else (world
               (pix->x x) (pix->y y)
               (if (not (find-figure
                            (world-figures ws)
                            (pix->x x)
                            (pix->y y)
                            (get-cur-col ws)))
                   (world-selx ws)
                   (pix->x x)) 
               (if (not (find-figure
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
                                            'queen)]
                           [(<= (/ height 2)
                                y
                                (+ (/ height 2) sq-size))
                            (turn-enpassant (world-figures ws)
                                            (get-cur-col ws)
                                            'rook)]
                           [(>= (/ height 2)
                                y
                                (- (/ height 2) sq-size))
                            (turn-enpassant (world-figures ws)
                                            (get-cur-col ws)
                                            'bishop)]
                           [(>= (- (/ height 2) sq-size)
                                y
                                (- (/ height 2) (* 2 sq-size)))
                            (turn-enpassant (world-figures ws)
                                            (get-cur-col ws)
                                            'knight)]
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
                       (if (not (find-figure
                                    (world-figures ws)
                                    (pix->x x)
                                    (pix->y y)
                                    (get-cur-col ws)))
                           (world-selx ws)
                           (figure-x (find-figure
                                      (world-figures ws)
                                      (pix->x x)
                                      (pix->y y)
                                      (get-cur-col ws))))
                       (if (not (find-figure
                                    (world-figures ws)
                                    (pix->x x)
                                    (pix->y y)
                                    (get-cur-col ws)))
                           (world-sely ws)
                           (figure-y (find-figure
                                      (world-figures ws)
                                      (pix->x x)
                                      (pix->y y)
                                      (get-cur-col ws))))
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
  (if (white? (figure-color fig))
      (cond
        [(eq? (figure-type fig) 'pawn)
         (bitmap/file "images/white/Chess_plt60.png")]
        [(eq? (figure-type fig) 'knight)
         (bitmap/file "images/white/Chess_nlt60.png")]
        [(eq? (figure-type fig) 'bishop)
         (bitmap/file "images/white/Chess_blt60.png")]
        [(eq? (figure-type fig) 'rook)
         (bitmap/file "images/white/Chess_rlt60.png")]
        [(eq? (figure-type fig) 'queen)
         (bitmap/file "images/white/Chess_qlt60.png")]
        [(eq? (figure-type fig) 'king)
         (bitmap/file "images/white/Chess_klt60.png")]
        [else (rectangle sq-size sq-size 'solid 'pink)]
        )
      (cond
        [(eq? (figure-type fig) 'pawn)
         (bitmap/file "images/black/Chess_pdt60.png")]
        [(eq? (figure-type fig) 'knight)
         (bitmap/file "images/black/Chess_ndt60.png")]
        [(eq? (figure-type fig) 'bishop)
         (bitmap/file "images/black/Chess_bdt60.png")]
        [(eq? (figure-type fig) 'rook)
         (bitmap/file "images/black/Chess_rdt60.png")]
        [(eq? (figure-type fig) 'queen)
         (bitmap/file "images/black/Chess_qdt60.png")]
        [(eq? (figure-type fig) 'king)
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
       (if (white? (get-cur-col ws))
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
