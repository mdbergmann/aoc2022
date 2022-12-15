(eval-when (:compile-toplevel)
  (ql:quickload :fiveam)
  (ql:quickload :str))

(defpackage :aoc2022.day10
  (:use :cl :fiveam)
  (:nicknames :day10)
  ;;(:export #:)
  )

(in-package :aoc2022.day10)

(defun demo-input ()
  (str:split #\NewLine *demo-input*))

(defun day10 (input)
  (let ((cycles '(20 60 100 140 180 220)))
    (loop :with cycle = 0
          :with reg = 1
          :with adder = 0
          :with cycle-step = (car cycles)
          :with update-reg = nil
          :for x :in input
          :until (null (car cycles))
          :if (str:starts-with-p "noop" x)
            :do (progn
                  (incf cycle)
                  (setf update-reg nil))
          :else
            :do (destructuring-bind (op incval)
                    (str:split " " x)
                  (declare (ignore op))
                  (setf adder (parse-integer incval)
                        update-reg t)
                  (incf cycle 2))
          :if (> cycle (1- cycle-step))
            :collect `(,reg . ,cycle-step)
            :and :do
              (progn
                (setf cycles (cdr cycles))
                (setf cycle-step (car cycles)))
          :if update-reg
            :do (incf reg adder))))

(test day10-1-demo
  (let ((result (day10 (demo-input))))
    (print result)
    (is (= 13140 (reduce #'+
                         (mapcar (lambda (x) (* (car x) (cdr x))) result)
                         :initial-value 0))))
  )

(run! 'day10-1-demo)

(defun real-input ()
  (str:split #\NewLine (str:from-file #P"../input/day10_1.txt")))

(test day10-1-real
  (let ((result (day10 (real-input))))
    (print result)
    (is (= 17020 (reduce #'+
                         (mapcar (lambda (x) (* (car x) (cdr x))) result)
                         :initial-value 0))))
  )

(run! 'day10-1-real)

(defparameter *demo-input*
  "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop")
