;; ШАБЛОНЫ:
;; Цель (goal) - вектор, состоящий из четырех компонентов:
;; действие, которое нужно выполнить,
;; объект, над которым должно быть выполнено действие;
;; исходное положение;
;; положение, в которое нужно перейти.
(deftemplate goal
	(field action (type SYMBOL))
	(field object (type SYMBOL))
	(field from (type SYMBOL))
	(field to (type SYMBOL))
)
;;Вектор 'in' указывает, где находится объект.
(deftemplate in (field object (type SYMBOL)) (field location (type SYMBOL)))
;; ФАКТЫ
;; Функция 'deffacts' вводит в рабочую память описание
;;исходного положения и вызывается при перезапуске системы.
;; Исходное состояние объектов следующее:
;; робот находится в комнате А;
;; ящик находится в комнате В.
;; Цель - переместить ящик в комнату А.
(deffacts world
	(in (object robot) (location RoomA))
	(in (object box) (location RoomB))
	(goal (action push) (object box) (from RoomB) (to RoomA))
)
;; ПРАВИЛА:
;; 1. Прекратить процесс, когда цель будет достигнута.
(defrule stop
	(goal (object ?X) (to ?Y))
	(in (object ?X) (location ?Y))
	=>
	(halt)
	(printout t "Object " ?X " was transfered in location " ?Y crlf)
)
;; 2. Если робот отсутствует в той комнате, где находится
;; ящик, который нужно передвинуть, то переместить туда робот.
(defrule move
	(goal (object ?X) (from ?Y))
	(in (object ?X) (location ?Y))
	?robot-position <- (in (object robot) (location ?Z&~?Y))
	=>
	(modify ?robot-position (location ?Y))
	(printout t "Robot moved into " ?Y crlf)
)

;; 3. Если робот и объект находятся не в той комнате, которая
;; указана в цели, то переместить туда робот вместе с объектом
(defrule push
	(goal (object ?X) (from ?Y) (to ?Z))
	(in (object ?X) (location ?Y))
	?object-position <- (in (object ?X) (location ?Y))
	?robot-position <- (in (object robot) (location ?Y))
	=>
	(modify ?robot-position (location ?Z))
	(modify ?object-position (location ?Z))
	(printout t "Robot pushed "?X " into "?Z crlf)
)