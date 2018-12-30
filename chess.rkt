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


;;; Модуль ускорения при нажатии на кнопку "вперед".
(define key-accel 2.0)

;;; Модуль угловой скорости при нажатии на кнопки поворота.
(define key-ω (/ pi 50))

;;; Множитель для гашения скорости.
(define damp 0.9)

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
(define (find-figure w x y col)
  (filter
   (lambda (cur)
     (and (= (figure-x cur) x)
          (= (figure-y cur) y)
          (= (figure-color cur) col)))
   (world-figures w)))
(define (find-figure-bytype w type col)
  (filter
   (lambda (cur)
     (and (= (figure-type cur) type)
          (= (figure-color cur) col)))
   (world-figures w)))
(define (isblack? x y)
  (even?  (+ x y)))
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
(define (draw w)
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
         ;;Доска
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
;; Возможно ли сходить фигурой в x y без учета шахов и матов
;(define (move? ws fig x y)
;  (cond
;    [(= (figure-type fig) 1)
;     (if (even? (figure-color fig))
;         (cond
;           ]
;    [else #f]))

;; Обработчик мыши
(define (mouse-handler ws x y event)
  (if (string=? event "button-down")
      (world (pix->x x) (pix->y y)
             (if (empty? (find-figure
                          ws
                          (pix->x x)
                          (pix->y y)
                          (modulo (world-number-of-move ws) 2)))
                 (world-selx ws)
                 (figure-x (car (find-figure
                                 ws
                                 (pix->x x)
                                 (pix->y y)
                                 (modulo (world-number-of-move ws) 2)))))
             (if (empty? (find-figure
                          ws
                          (pix->x x)
                          (pix->y y)
                          (modulo (world-number-of-move ws) 2)))
                 (world-sely ws)
                 (figure-y (car (find-figure
                                 ws
                                 (pix->x x)
                                 (pix->y y)
                                 (modulo (world-number-of-move ws) 2)))))
             (world-figures ws)
             (world-number-of-move ws))
      ws
      ))
(define (start)
  (big-bang world0
            (on-mouse mouse-handler)
           (on-draw draw)))
