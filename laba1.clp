(defclass Question
    (is-a USER)
    (role concrete)
    (pattern-match reactive)

    (slot questionText (type STRING) (create-accessor read-write))
    (slot answerVariants (type STRING) (create-accessor read-write))
    (slot rightAnswer (type STRING) (create-accessor read-write))
    (slot weight (type FLOAT) (create-accessor read-write))
)

(definstances Questions
  (question1 of Question
    (questionText "The only object capable of thinking is a human brain. Therefore, any 'thinking' device should somehow reproduce the structure of the human brain. This is the main postulate:")
    (answerVariants "1) Cybernetics of the Black Box 2) Logic-Symbolic IP 3) Neurinformatics 4) Robotics")
    (rightAnswer 3)
    (weight 10.0)
  )

  (question2 of Question
    (questionText "Main component of IP, multiple rules, heuristics, procedures:")
    (answerVariants "1) Database 2) Knowledge Base 3) Library 4) Interpreter 5) Expert System")
    (rightAnswer 2)
    (weight 10.0)
  )
  
  (question3 of Question
    (questionText "Movement in structured space from some nodes of this space to another on a specific strategy:")
    (answerVariants "1) Game 2) Travel 3) Search 4) Flight")
    (rightAnswer 3)
    (weight 10.0)
  )

  (question4 of Question
    (questionText "Knowledge is:")
    (answerVariants "1) Processed data 2) Processed information 3) Signals 4) Facts 5) Objects 6) Properties of objects ")
    (rightAnswer 2)
    (weight 10.0)
  )

  (question5 of Question
    (questionText "Considering that each symbol is encoded by one byte, rate the information volume of the proposal: 'My uncle of the most honest rules, when the Zalenor is not a joke, he forced himself and better to invent could not':")
    (answerVariants "1) 108 bytes 2) 264 bytes 3) 108 kilobyte 4) 864 kilobyte")
    (rightAnswer 1)
    (weight 10.0)
  )

  (question6 of Question
    (questionText "A person interacts with one computer and one person. Based on the answers to questions, he must determine who he is talking to: with a person or computer program. The task of the computer program is to mislead a person, forcing you to make an incorrect choice. This is a test:")
    (answerVariants "1) Bul 2) EGE 3) Lucher 4) Miller-Rabin 5) Turing 6) IQ")
    (rightAnswer 5)
    (weight 10.0)
  )

  (question7 of Question
    (questionText "When studying a certain object, the concepts appear in the following order:")
    (answerVariants "1) Knowledge-information-data 2) Data-information-knowledge 3) Information-Data-Knowledge 4) Knowledge-data-information")
    (rightAnswer 2)
    (weight 10.0)
  )

  (question8 of Question
    (questionText "To fulfill the product rule, it is necessary:")
    (answerVariants "1) Execution of at least one of its conditions 2) Performing at least one of its conclusions 3) Performing all its conditions 4) fulfillment of all its conclusions")
    (rightAnswer 3)
    (weight 10.0)
  )

  (question9 of Question
    (questionText "The maximum size of the knowledge base in the production model does not exceed:")
    (answerVariants "1) 10 entries 2) 100 entries 3) 1000 entries 4) 65 534 entries")
    (rightAnswer 3)
    (weight 10.0)
  )

  (question10 of Question
    (questionText "Conditional statement on the left side of the product rule, which must be performed in the working memory in order for the appropriate actions in the right-hand side of the rule:")
    (answerVariants "1) Antecedent 2) Dependence 3) Consection 4) Resolution 5) Slimogism ")
    (rightAnswer 1)
    (weight 10.0)
  )
)

(defclass Student
  (is-a USER)
  (role concrete)
  (pattern-match reactive)

  (slot firstName (type STRING) (create-accessor read-write))
  (slot lastName (type STRING) (create-accessor read-write))
  (slot patronymic (type STRING) (create-accessor read-write))
  (slot questionAnswer1 (type NUMBER) (create-accessor read-write))
  (slot questionAnswer2 (type NUMBER) (create-accessor read-write))
  (slot questionAnswer3 (type NUMBER) (create-accessor read-write))
  (slot questionAnswer4 (type NUMBER) (create-accessor read-write))
  (slot questionAnswer5 (type NUMBER) (create-accessor read-write))
  (slot questionAnswer6 (type NUMBER) (create-accessor read-write))
  (slot questionAnswer7 (type NUMBER) (create-accessor read-write))
  (slot questionAnswer8 (type NUMBER) (create-accessor read-write))
  (slot questionAnswer9 (type NUMBER) (create-accessor read-write))
  (slot questionAnswer10 (type NUMBER) (create-accessor read-write))
)

(definstances Student
  (student of Student
    (firstName "")
    (lastName "")
    (patronymic "")
    (questionAnswer1 0)
    (questionAnswer2 0)
    (questionAnswer3 0)
    (questionAnswer4 0)
    (questionAnswer5 0)
    (questionAnswer6 0)
    (questionAnswer7 0)
    (questionAnswer8 0)
    (questionAnswer9 0)
    (questionAnswer10 0)
  )
)

(deftemplate selectRange
  (field name (type STRING) (default ""))
  (field num (type NUMBER) (default 0))
  (field minCount (type FLOAT))
)

(deftemplate asker
  (field studFirstName (type STRING) (default ""))
  (field studLastName (type STRING) (default ""))
  (field studPatronymic (type STRING) (default ""))
  (field curQuestionNum (type INTEGER) (default 0))
  (field curQuestionAnsw (type INTEGER) (default 0))
  (field totalMark (type FLOAT) (default 0.0))
  (field maxPossibleWeight (type FLOAT) (default 0.0))
)

(defrule start
=>
  (printout t "Enter your FIO and select range of grade: " crlf)
  (assert(asker(studFirstName(read))(studLastName(read))(studPatronymic(read))))
  (printout t "Press 2 to select: unsatisfactory (0% - 55%)" crlf)
  (printout t "Press 3 to select: satisfactory (55% - 70%)" crlf)
  (printout t "Press 4 to select: good (70% - 85%)" crlf)
  (printout t "Press 5 to select: excellent (85% - 100%)" crlf)
  (assert(selectRange(num(read))))
)

(defrule setRange
  ?PesSel <- (selectRange (num ?num) (name ""))
  ?answeredQuestion <- (asker (curQuestionNum ?cur))
=>
  (if (= ?num 2)
  then 
    (modify ?PesSel (name "unsatisfactory") (minCount 0.0))
    (printout t "the assessment is confirmed" crlf)
    (halt) 
  else (if (= ?num 3) then
    (modify ?PesSel (name "satisfactory") (minCount 60.0))
    (printout t "you need 6 right answer" crlf)
  else (if (= ?num 4) then
    (modify ?PesSel (name "good") (minCount 70.0))
    (printout t "you need 7 to 8 right answer" crlf)
  else 
    (modify ?PesSel (name "excellent") (minCount 90.0))
    (printout t "you need 9 to 10 right answer" crlf)
  )))
)

(defrule setFIO
  (asker (studFirstName ?x1) (studLastName ?x2) (studPatronymic ?x3))
  (object (name [student]) (firstName "") (lastName "") (patronymic ""))
=>
  (send [student] changeFIO ?x1 ?x2 ?x3)
)

(
  defmessage-handler Student changeFIO (?inputFisrtName ?inputLastName ?inputPatronymic)
  (dynamic-put firstName ?inputFisrtName)
  (dynamic-put lastName ?inputLastName)
  (dynamic-put patronymic ?inputPatronymic)
)


; ;;;;; ASK QUESTIONS

;;; Ask question1 and adds it to the template 'asker'
(defrule askQuestion1
  ?askedQuestion <- (asker(curQuestionNum 0))
  (object (name [question1]) (questionText ?questionText) (answerVariants ?answerVariants))
=>
  (printout t ?questionText crlf)
  (printout t ?answerVariants crlf)
  (printout t "Your answer: ")
  (modify ?askedQuestion (curQuestionNum 1) (curQuestionAnsw(read)))
  (printout t crlf)
)

;;; Count received weight from question1 
;;; and add the results to the template 'asker'
(defrule setAnswQuestion1
  ?answeredQuestion <- (asker (curQuestionNum 1) (curQuestionAnsw ?curAnsw) (totalMark ?totalMark) (maxPossibleWeight ?maxWeight))
  (object (name [student]) (questionAnswer1 0))
  (object (name [question1]) (rightAnswer ?rightAnsw) (weight ?questionWeight))
  (selectRange(minCount ?min))
=> 
  (if (eq ?curAnsw ?rightAnsw) 
    then (
      modify ?answeredQuestion(totalMark(+ ?totalMark ?questionWeight))(maxPossibleWeight(+ ?maxWeight ?questionWeight)))
      (if (= (+ ?totalMark ?questionWeight) ?min) then 
      (printout t "the assessment is confirmed" crlf)
      (halt))  
    else 
      (modify ?answeredQuestion(maxPossibleWeight(+ ?maxWeight ?questionWeight)))
      (if (= (- 100.0 (- (+ ?maxWeight ?questionWeight) (+ ?totalMark ?questionWeight))) ?min) then 
      (printout t "the assessment is not confirmed" crlf)
      (halt)) 
  )
  (send [student] setStudentQuestionAnswer1 ?curAnsw)
)

;;; Add number of answered question1 to Student object
(
  defmessage-handler Student setStudentQuestionAnswer1 (?answeredNumber)
  (dynamic-put questionAnswer1 ?answeredNumber)
)


;;; Ask question2 and adds it to the template 'asker'
(defrule askQuestion2
  ?askedQuestion <- (asker(curQuestionNum 1))
  (object (name [question2]) (questionText ?questionText) (answerVariants ?answerVariants))
=>
  (printout t ?questionText crlf)
  (printout t ?answerVariants crlf)
  (printout t "Your answer: ")
  (modify ?askedQuestion (curQuestionNum 2) (curQuestionAnsw(read)))
  (printout t crlf)
)

;;; Count received weight from question2
;;; and add the results to the template 'asker'
(defrule setAnswQuestion2
  ?answeredQuestion <- (asker (curQuestionNum 2) (curQuestionAnsw ?curAnsw) (totalMark ?totalMark) (maxPossibleWeight ?maxWeight))
  (object (name [student]) (questionAnswer2 0))
  (object (name [question2]) (rightAnswer ?rightAnsw) (weight ?questionWeight))
  (selectRange(minCount ?min))
=> 
  (
    if (eq ?curAnsw ?rightAnsw) 
    then 
      (modify ?answeredQuestion(totalMark(+ ?totalMark ?questionWeight))(maxPossibleWeight(+ ?maxWeight ?questionWeight)))
       (if (= (+ ?totalMark ?questionWeight) ?min) then 
      (printout t "the assessment is confirmed" crlf)
      (halt))   
      else 
      (modify ?answeredQuestion(maxPossibleWeight(+ ?maxWeight ?questionWeight)))
      (if (= (- 100.0 (- (+ ?maxWeight ?questionWeight) (+ ?totalMark ?questionWeight))) ?min) then 
      (printout t "the assessment is not confirmed" crlf)
      (halt)) 
  )
  (send [student] setStudentQuestionAnswer2 ?curAnsw)
  
)
;;; Add number of answered question2 to Student object
(
  defmessage-handler Student setStudentQuestionAnswer2 (?answeredNumber)
  (dynamic-put questionAnswer2 ?answeredNumber)
)


;;; Ask question3 and adds it to the template 'asker'
(defrule askQuestion3
  ?askedQuestion <- (asker(curQuestionNum 2))
  (object (name [question3]) (questionText ?questionText) (answerVariants ?answerVariants))
=>
  (printout t ?questionText crlf)
  (printout t ?answerVariants crlf)
  (printout t "Your answer: ")
  (modify ?askedQuestion (curQuestionNum 3) (curQuestionAnsw(read)))
  (printout t crlf)
)

;;; Count received weight from question3
;;; and add the results to the template 'asker'
(defrule setAnswQuestion3
  ?answeredQuestion <- (asker (curQuestionNum 3) (curQuestionAnsw ?curAnsw) (totalMark ?totalMark) (maxPossibleWeight ?maxWeight))
  (object (name [student]) (questionAnswer3 0))
  (object (name [question3]) (rightAnswer ?rightAnsw) (weight ?questionWeight))
  (selectRange(minCount ?min))
=> 
  (
    if (eq ?curAnsw ?rightAnsw) 
    then (
      modify ?answeredQuestion(totalMark(+ ?totalMark ?questionWeight))(maxPossibleWeight(+ ?maxWeight ?questionWeight))) 
      (if (= (+ ?totalMark ?questionWeight) ?min) then 
      (printout t "the assessment is confirmed" crlf)
      (halt)) 
      else 
      (modify ?answeredQuestion(maxPossibleWeight(+ ?maxWeight ?questionWeight)))
      (if (= (- 100.0 (- (+ ?maxWeight ?questionWeight) (+ ?totalMark ?questionWeight))) ?min) then 
      (printout t "the assessment is not confirmed" crlf)
      (halt)) 
    
  )
  (send [student] setStudentQuestionAnswer3 ?curAnsw)
)

;;; Add number of answered question3 to Student object
(
  defmessage-handler Student setStudentQuestionAnswer3 (?answeredNumber)
  (dynamic-put questionAnswer3 ?answeredNumber)
)


;;; Ask question4 and adds it to the template 'asker'
(defrule askQuestion4
  ?askedQuestion <- (asker(curQuestionNum 3))
  (object (name [question4]) (questionText ?questionText) (answerVariants ?answerVariants))
=>
  (printout t ?questionText crlf)
  (printout t ?answerVariants crlf)
  (printout t "Your answer: ")
  (modify ?askedQuestion (curQuestionNum 4) (curQuestionAnsw(read)))
  (printout t crlf)
)

;;; Count received weight from question4
;;; and add the results to the template 'asker'
(defrule setAnswQuestion4
  ?answeredQuestion <- (asker (curQuestionNum 4) (curQuestionAnsw ?curAnsw) (totalMark ?totalMark) (maxPossibleWeight ?maxWeight))
  (object (name [student]) (questionAnswer4 0))
  (object (name [question4]) (rightAnswer ?rightAnsw) (weight ?questionWeight))
  (selectRange(minCount ?min))
=> 
  (
    if (eq ?curAnsw ?rightAnsw) 
    then (
      modify ?answeredQuestion(totalMark(+ ?totalMark ?questionWeight))(maxPossibleWeight(+ ?maxWeight ?questionWeight)))
       (if (= (+ ?totalMark ?questionWeight) ?min) then 
      (printout t "the assessment is confirmed" crlf)
      (halt)
      )  
    else 
    (modify ?answeredQuestion(maxPossibleWeight(+ ?maxWeight ?questionWeight)))
      (if (= (- 100.0 (- (+ ?maxWeight ?questionWeight) (+ ?totalMark ?questionWeight))) ?min) then 
      (printout t "the assessment is not confirmed" crlf)
      (halt)) 
  )
  (send [student] setStudentQuestionAnswer4 ?curAnsw)
)

;;; Add number of answered question4 to Student object
(
  defmessage-handler Student setStudentQuestionAnswer4 (?answeredNumber)
  (dynamic-put questionAnswer4 ?answeredNumber)
)


;;; Ask question5 and adds it to the template 'asker'
(defrule askQuestion5
  ?askedQuestion <- (asker(curQuestionNum 4))
  (object (name [question5]) (questionText ?questionText) (answerVariants ?answerVariants))
=>
  (printout t ?questionText crlf)
  (printout t ?answerVariants crlf)
  (printout t "Your answer: ")
  (modify ?askedQuestion (curQuestionNum 5) (curQuestionAnsw(read)))
  (printout t crlf)
)

;;; Count received weight from question5
;;; and add the results to the template 'asker'
(defrule setAnswQuestion5
  ?answeredQuestion <- (asker (curQuestionNum 5) (curQuestionAnsw ?curAnsw) (totalMark ?totalMark) (maxPossibleWeight ?maxWeight))
  (object (name [student]) (questionAnswer5 0))
  (object (name [question5]) (rightAnswer ?rightAnsw) (weight ?questionWeight))
  (selectRange(minCount ?min))
=> 
  (
    if (eq ?curAnsw ?rightAnsw) 
    then (
      modify ?answeredQuestion(totalMark(+ ?totalMark ?questionWeight))(maxPossibleWeight(+ ?maxWeight ?questionWeight)))
       (if (= (+ ?totalMark ?questionWeight) ?min) then 
      (printout t "the assessment is confirmed" crlf)
      (halt)
      )  
    else 
    (modify ?answeredQuestion(maxPossibleWeight(+ ?maxWeight ?questionWeight)))
      (if (= (- 100.0 (- (+ ?maxWeight ?questionWeight) (+ ?totalMark ?questionWeight))) ?min) then 
      (printout t "the assessment is not confirmed" crlf)
      (halt)) 
  )
  (send [student] setStudentQuestionAnswer5 ?curAnsw)
)

;;; Add number of answered question5 to Student object
(
  defmessage-handler Student setStudentQuestionAnswer5 (?answeredNumber)
  (dynamic-put questionAnswer5 ?answeredNumber)
)


;;; Ask question6 and adds it to the template 'asker'
(defrule askQuestion6
  ?askedQuestion <- (asker(curQuestionNum 5))
  (object (name [question6]) (questionText ?questionText) (answerVariants ?answerVariants))
=>
  (printout t ?questionText crlf)
  (printout t ?answerVariants crlf)
  (printout t "Your answer: ")
  (modify ?askedQuestion (curQuestionNum 6) (curQuestionAnsw(read)))
  (printout t crlf)
)

;;; Count received weight from question6
;;; and add the results to the template 'asker'
(defrule setAnswQuestion6
  ?answeredQuestion <- (asker (curQuestionNum 6) (curQuestionAnsw ?curAnsw) (totalMark ?totalMark) (maxPossibleWeight ?maxWeight))
  (object (name [student]) (questionAnswer6 0))
  (object (name [question6]) (rightAnswer ?rightAnsw) (weight ?questionWeight))
  (selectRange(minCount ?min))
=> 
  (
    if (eq ?curAnsw ?rightAnsw) 
    then (
      modify ?answeredQuestion(totalMark(+ ?totalMark ?questionWeight))(maxPossibleWeight(+ ?maxWeight ?questionWeight)))
      (if (= (+ ?totalMark ?questionWeight) ?min) then 
      (printout t "the assessment is confirmed" crlf)
      (halt)
      )  
    else 
    (modify ?answeredQuestion(maxPossibleWeight(+ ?maxWeight ?questionWeight)))
      (if (= (- 100.0 (- (+ ?maxWeight ?questionWeight) (+ ?totalMark ?questionWeight))) ?min) then 
      (printout t "the assessment is not confirmed" crlf)
      (halt)) 
  )
  (send [student] setStudentQuestionAnswer6 ?curAnsw)
)

;;; Add number of answered question6 to Student object
(
  defmessage-handler Student setStudentQuestionAnswer6 (?answeredNumber)
  (dynamic-put questionAnswer6 ?answeredNumber)
)


;;; Ask question7 and adds it to the template 'asker'
(defrule askQuestion7
  ?askedQuestion <- (asker(curQuestionNum 6))
  (object (name [question7]) (questionText ?questionText) (answerVariants ?answerVariants))
=>
  (printout t ?questionText crlf)
  (printout t ?answerVariants crlf)
  (printout t "Your answer: ")
  (modify ?askedQuestion (curQuestionNum 7) (curQuestionAnsw(read)))
  (printout t crlf)
)

;;; Count received weight from question7
;;; and add the results to the template 'asker'
(defrule setAnswQuestion7
  ?answeredQuestion <- (asker (curQuestionNum 7) (curQuestionAnsw ?curAnsw) (totalMark ?totalMark) (maxPossibleWeight ?maxWeight))
  (object (name [student]) (questionAnswer7 0))
  (object (name [question7]) (rightAnswer ?rightAnsw) (weight ?questionWeight))
  (selectRange(minCount ?min))
=> 
  (
    if (eq ?curAnsw ?rightAnsw) 
    then (
      modify ?answeredQuestion(totalMark(+ ?totalMark ?questionWeight))(maxPossibleWeight(+ ?maxWeight ?questionWeight))) 
      (if (= (+ ?totalMark ?questionWeight) ?min) then 
      (printout t "the assessment is confirmed" crlf)
      (halt)
      ) 
    else 
    (modify ?answeredQuestion(maxPossibleWeight(+ ?maxWeight ?questionWeight)))
      (if (= (- 100.0 (- (+ ?maxWeight ?questionWeight) (+ ?totalMark ?questionWeight))) ?min) then 
      (printout t "the assessment is not confirmed" crlf)
      (halt)) 
  )
  (send [student] setStudentQuestionAnswer7 ?curAnsw)
)

;;; Add number of answered question7 to Student object
(
  defmessage-handler Student setStudentQuestionAnswer7 (?answeredNumber)
  (dynamic-put questionAnswer7 ?answeredNumber)
)


;;; Ask question8 and adds it to the template 'asker'
(defrule askQuestion8
  ?askedQuestion <- (asker(curQuestionNum 7))
  (object (name [question8]) (questionText ?questionText) (answerVariants ?answerVariants))
=>
  (printout t ?questionText crlf)
  (printout t ?answerVariants crlf)
  (printout t "Your answer: ")
  (modify ?askedQuestion (curQuestionNum 8) (curQuestionAnsw(read)))
  (printout t crlf)
)

;;; Count received weight from question8
;;; and add the results to the template 'asker'
(defrule setAnswQuestion8
  ?answeredQuestion <- (asker (curQuestionNum 8) (curQuestionAnsw ?curAnsw) (totalMark ?totalMark) (maxPossibleWeight ?maxWeight))
  (object (name [student]) (questionAnswer8 0))
  (object (name [question8]) (rightAnswer ?rightAnsw) (weight ?questionWeight))
  (selectRange(minCount ?min))
=> 
  (
    if (eq ?curAnsw ?rightAnsw) 
    then (
      modify ?answeredQuestion(totalMark(+ ?totalMark ?questionWeight))(maxPossibleWeight(+ ?maxWeight ?questionWeight))) 
      (if (= (+ ?totalMark ?questionWeight) ?min) then 
      (printout t "the assessment is confirmed" crlf)
      (halt)
      ) 

      else 
      (modify ?answeredQuestion(maxPossibleWeight(+ ?maxWeight ?questionWeight)))
      (if (= (- 100.0 (- (+ ?maxWeight ?questionWeight) (+ ?totalMark ?questionWeight))) ?min) then 
      (printout t "the assessment is not confirmed" crlf)
      (halt)) 
  )
  (send [student] setStudentQuestionAnswer8 ?curAnsw)
)

;;; Add number of answered question 8 to Student object
(
  defmessage-handler Student setStudentQuestionAnswer8 (?answeredNumber)
  (dynamic-put questionAnswer8 ?answeredNumber)
)


;;; Ask question9 and adds it to the template 'asker'
(defrule askQuestion9
  ?askedQuestion <- (asker(curQuestionNum 8))
  (object (name [question9]) (questionText ?questionText) (answerVariants ?answerVariants))
=>
  (printout t ?questionText crlf)
  (printout t ?answerVariants crlf)
  (printout t "Your answer: ")
  (modify ?askedQuestion (curQuestionNum 9) (curQuestionAnsw(read)))
  (printout t crlf)
)

;;; Count received weight from question9
;;; and add the results to the template 'asker'
(defrule setAnswQuestion9
  ?answeredQuestion <- (asker (curQuestionNum 9) (curQuestionAnsw ?curAnsw) (totalMark ?totalMark) (maxPossibleWeight ?maxWeight))
  (object (name [student]) (questionAnswer9 0))
  (object (name [question9]) (rightAnswer ?rightAnsw) (weight ?questionWeight))
  (selectRange(minCount ?min))
=> 
  (
    if (eq ?curAnsw ?rightAnsw) 
    then (
      modify ?answeredQuestion(totalMark(+ ?totalMark ?questionWeight))(maxPossibleWeight(+ ?maxWeight ?questionWeight))) 
      (if (= (+ ?totalMark ?questionWeight) ?min) then 
      (printout t "the assessment is confirmed" crlf)
      (halt)) 
    else 
    (modify ?answeredQuestion(maxPossibleWeight(+ ?maxWeight ?questionWeight)))
      (if (= (- 100.0 (- (+ ?maxWeight ?questionWeight) (+ ?totalMark ?questionWeight))) ?min) then 
      (printout t "the assessment is not confirmed" crlf)
      (halt)) 
  )
  (send [student] setStudentQuestionAnswer9 ?curAnsw)
)

;;; Add number of answered question 9 to Student object
(
  defmessage-handler Student setStudentQuestionAnswer9 (?answeredNumber)
  (dynamic-put questionAnswer9 ?answeredNumber)
)


;;; Ask question10 and adds it to the template 'asker'
(defrule askQuestion10
  ?askedQuestion <- (asker(curQuestionNum 9))
  (object (name [question10]) (questionText ?questionText) (answerVariants ?answerVariants))
=>
  (printout t ?questionText crlf)
  (printout t ?answerVariants crlf)
  (printout t "Your answer: ")
  (modify ?askedQuestion (curQuestionNum 10) (curQuestionAnsw(read)))
  (printout t crlf)
)

;;; Count received weight from question10
;;; and add the results to the template 'asker'
(defrule setAnswQuestion10
  ?answeredQuestion <- (asker (curQuestionNum 10) (curQuestionAnsw ?curAnsw) (totalMark ?totalMark) (maxPossibleWeight ?maxWeight))
  (object (name [student]) (questionAnswer10 0))
  (object (name [question10]) (rightAnswer ?rightAnsw) (weight ?questionWeight))
  (selectRange(minCount ?min))
=> 
  (
    if (eq ?curAnsw ?rightAnsw) 
    then (
      modify ?answeredQuestion(totalMark(+ ?totalMark ?questionWeight))(maxPossibleWeight(+ ?maxWeight ?questionWeight))) 
      (if (= (+ ?totalMark ?questionWeight) ?min) then 
      (printout t "the assessment is confirmed" crlf)
      (halt)
      ) 
    else 
    (modify ?answeredQuestion(maxPossibleWeight(+ ?maxWeight ?questionWeight)))
      (if (= (- 100.0 (- (+ ?maxWeight ?questionWeight) (+ ?totalMark ?questionWeight))) ?min) then 
      (printout t "the assessment is not confirmed" crlf)
      (halt)) 
  )
  (send [student] setStudentQuestionAnswer10 ?curAnsw)
)

;;; Add number of answered question 10 to Student object
(
  defmessage-handler Student setStudentQuestionAnswer10 (?answeredNumber)
  (dynamic-put questionAnswer10 ?answeredNumber)
)