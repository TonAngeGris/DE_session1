(defclass game
	(is-a USER)
	(role concrete)
	(pattern-match reactive)
	(slot genre (type SYMBOL)(create-accessor read-write))
	(slot yearOfRelease (type INTEGER)(create-accessor read-write))
	(slot graphicalMemory (type INTEGER)(create-accessor read-write))
	(slot RAM (type INTEGER)(create-accessor read-write))
	(slot company (type SYMBOL)(create-accessor read-write))
	(slot numberOfGames (type INTEGER)(create-accessor read-write))
	(slot networkMode (type SYMBOL)(create-accessor read-write))
	(slot price (type INTEGER)(create-accessor read-write))
)

(definstances game
	(Witcher3 of game
		(genre Action)
		(yearOfRelease 2015)
		(graphicalMemory 2048)
		(RAM 6)
		(company CDProjectRed)
		(numberOfGames 3)
		(networkMode no)
		(price 1200))
		
	(TES5:Skyrim of game
		(genre Adventure)
		(yearOfRelease 2011)
		(graphicalMemory 1024)
		(RAM 2)
		(company Bethesda)
		(numberOfGames 5)
		(networkMode no)
		(price 1600))
	
	(King'sBounty:TheLegend of game
		(genre Strategy)
		(yearOfRelease 2008)
		(graphicalMemory 128)
		(RAM 1)
		(company 1C)
		(numberOfGames 6)
		(networkMode no)
		(price 150))
		
	(LittleNightmares of game
		(genre Horror)
		(yearOfRelease 2017)
		(graphicalMemory 512)
		(RAM 1)
		(company BandaiNamco)
		(numberOfGames 2)
		(networkMode no)
		(price 850))
		
	(ForzaHorizon5 of game
		(genre Race)
		(yearOfRelease 2021)
		(graphicalMemory 2048)
		(RAM 8)
		(company PlayGroundGames)
		(numberOfGames 5)
		(networkMode yes)
		(price 425))
	
	(FarCry6 of game
		(genre Shooter)
		(yearOfRelease 2021)
		(graphicalMemory 4096)
		(RAM 16)
		(company Ubisoft)
		(numberOfGames 6)
		(networkMode yes)
		(price 2500))
		
	(AmongUs of game
		(genre Simulator)
		(yearOfRelease 2018)
		(graphicalMemory 128)
		(RAM 1)
		(company Innersloth)
		(numberOfGames 1)
		(networkMode yes)
		(price 133))
		
	(EverlastingSummer of game
		(genre Novel)
		(yearOfRelease 2013)
		(graphicalMemory 512)
		(RAM 1)
		(company SovietGames)
		(numberOfGames 1)
		(networkMode no)
		(price 0))
		
	(FIFA22 of game
		(genre Football)
		(yearOfRelease 2021)
		(graphicalMemory 2048)
		(RAM 8)
		(company EA)
		(numberOfGames 29)
		(networkMode yes)
		(price 2100))
		
	(Don'tStarveTogether of game
		(genre Survival)
		(yearOfRelease 2016)
		(graphicalMemory 256)
		(RAM 1)
		(company KleiEntertainment)
		(numberOfGames 2)
		(networkMode yes)
		(price 350))
)

(defclass questions
    (is-a USER)
	(role concrete)
	(pattern-match reactive)
    (slot text (type STRING))
    (slot variants (type STRING)(default ""))
    (slot answer (type STRING))
)

(definstances questions
    (question1 of questions
        (text "What genre do you prefer?")
        (variants "Survival,Football,Race,Novel,Simulator,Shooter,Horror,Strategy,Adventure,Action")
    )
    (question2 of questions
        (text "What year should the game be from?")
    )
    (question3 of questions
        (text "How many graphical memory do you have(in MBs)?")
        (variants "multiple 128")
    )
    (question4 of questions
        (text "How many RAM do you have(in GBs)?")
        (variants "min 1")
    )
    (question5 of questions
        (text "What company do you prefer?")
        (variants "KleiEntertainment,EA,SovietGames,Innersloth,Ubisoft,Bethesda,CDProjectRed,1C,PlayGroundGames,BandaiNamco")
    )
    (question6 of questions
        (text "How many series should it be?")
        (variants "minumum number")
    )
    (question7 of questions
        (text "Should it have network mode?")
        (variants "yes,no")
    )
    (question8 of questions
        (text "How many money can you spend on it?")
        (variants "maximum number")
    )
)


(defclass buyer
	(is-a USER)
	(pattern-match reactive)
	(slot firstName (type STRING)(create-accessor read-write))
	(slot lastName (type STRING)(create-accessor read-write))
	(slot patronymic (type STRING)(create-accessor read-write))
	(slot preferences 
		(type INSTANCE)
		(allowed-classes GAME)
		)
		
	(slot yourGame 
		(type INSTANCE)
		(allowed-classes GAME)
		)
		
)

(definstances buyer
    (B of buyer
        (firstName "")
        (lastName "")
        (patronymic "")
        (preferences (make-instance [g1] of game))
        (yourGame (make-instance [g2] of game))
    )
)

(deftemplate questioner
    (field studName(type STRING)(default ""))
    (field studLastName(type STRING)(default ""))
    (field studPatronymic(type STRING)(default ""))
    (field questionNum(type INTEGER)(default 0))
    (field gameNumber(type INTEGER)(default 0))
    (field gameName(type STRING)(default ""))
)

(deffacts initList
    (initial-fact)
)

(defrule initFact
    (initial-fact)
    =>
    (printout t "Your FIO:")
    (assert(questioner(studName(read))(studLastName(read))(studPatronymic(read))))
)


(defrule setFIO
    (questioner(studName ?x) (studLastName ?y) (studPatronymic ?z))
    (object (name [B]) (firstName "") (lastName "") (patronymic ""))
    =>
    (send [B] changeFIO ?x ?y ?z)
)

(
    defmessage-handler buyer changeFIO (?studentNameInput ?studentLastNameInput ?studentPatronymicInput) 
    (dynamic-put firstName ?studentNameInput)
    (dynamic-put lastName ?studentLastNameInput)
    (dynamic-put patronymic ?studentPatronymicInput)
)


(defrule Q1
    ?currentQ <- (questioner(questionNum 0))
    (object (name [question1])(text ?text)(variants ?variants))
    (object (name [g1])(genre ?mypref))
    =>
    (printout t ?text crlf)
    (if (not(eq ?variants ""))
        then (
            printout t ?variants crlf
            )
    )
    (modify ?currentQ(questionNum 1))
    (send [g1] changeGenre)
)

(
    defmessage-handler game changeGenre () 
    (dynamic-put genre(read))
)

(defrule Q2
    ?currentQ <- (questioner(questionNum 1))
    (object (name [question2])(text ?text)(variants ?variants))
    (object (name [g1])(yearOfRelease ?mypref))
    =>
    (printout t ?text crlf)
    (if (not(eq ?variants ""))
        then (
            printout t ?variants crlf
            )
    )
    (modify ?currentQ(questionNum 2))
    (send [g1] changeYear)
)

(
    defmessage-handler game changeYear () 
    (dynamic-put yearOfRelease(read))
)

(defrule Q3
    ?currentQ <- (questioner(questionNum 2))
    (object (name [question3])(text ?text)(variants ?variants))
    (object (name [g1])(graphicalMemory ?mypref))
    =>
    (printout t ?text crlf)
    (if (not(eq ?variants ""))
        then (
            printout t ?variants crlf
            )
    )
    (modify ?currentQ(questionNum 3))
    (send [g1] changeGraphical)
)

(
    defmessage-handler game changeGraphical () 
    (dynamic-put graphicalMemory(read))
)

(defrule Q4
    ?currentQ <- (questioner(questionNum 3))
    (object (name [question4])(text ?text)(variants ?variants))
    (object (name [g1])(RAM ?mypref))
    =>
    (printout t ?text crlf)
    (if (not(eq ?variants ""))
        then (
            printout t ?variants crlf
            )
    )
    (modify ?currentQ(questionNum 4))
    (send [g1] changeRAM)
)

(
    defmessage-handler game changeRAM () 
    (dynamic-put RAM(read))
)

(defrule Q5
    ?currentQ <- (questioner(questionNum 4))
    (object (name [question5])(text ?text)(variants ?variants))
    (object (name [g1])(company ?mypref))
    =>
    (printout t ?text crlf)
    (if (not(eq ?variants ""))
        then (
            printout t ?variants crlf
            )
    )
    (modify ?currentQ(questionNum 5))
    (send [g1] changeCompany)
)

(
    defmessage-handler game changeCompany () 
    (dynamic-put company(read))
)

(defrule Q6
    ?currentQ <- (questioner(questionNum 5))
    (object (name [question6])(text ?text)(variants ?variants))
    (object (name [g1])(numberOfGames ?mypref))
    =>
    (printout t ?text crlf)
    (if (not(eq ?variants ""))
        then (
            printout t ?variants crlf
            )
    )
    (modify ?currentQ(questionNum 6))
    (send [g1] changeNumber)
)

(
    defmessage-handler game changeNumber () 
    (dynamic-put numberOfGames(read))
)

(defrule Q7
    ?currentQ <- (questioner(questionNum 6))
    (object (name [question7])(text ?text)(variants ?variants))
    (object (name [g1])(networkMode ?mypref))
    =>
    (printout t ?text crlf)
    (if (not(eq ?variants ""))
        then (
            printout t ?variants crlf
            )
    )
    (modify ?currentQ(questionNum 7))
    (send [g1] changeNetwork)
)

(
    defmessage-handler game changeNetwork () 
    (dynamic-put networkMode(read))
)

(defrule Q8
    ?currentQ <- (questioner(questionNum 7))
    (object (name [question8])(text ?text)(variants ?variants))
    (object (name [g1])(price ?mypref))
    =>
    (printout t ?text crlf)
    (if (not(eq ?variants ""))
        then (
            printout t ?variants crlf
            )
    )
    (modify ?currentQ(gameNumber 1))
    (modify ?currentQ(gameName "Witcher3"))
    (modify ?currentQ(questionNum 8))
    (send [g1] changePrice)
)

(
    defmessage-handler game changePrice () 
    (dynamic-put price(read))
)

(defrule Result
    ?currentQ <- (questioner(gameNumber 1)(gameName ?x))
    (object (name [?x])(price ?mypref))
    =>
    (printout t ?x crlf)
)