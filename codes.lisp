;#######################################   Fonction_main
(defun TP3 ()
(init)
(message-box (format nil "Mohamed SABT ~% System expert ~% Parc de Dinosaures ~% 28-12-2010") "Auteurs" :topmost t)
(loop with decision do
	(setq decision 	(parse-integer	(ask-user-for-string
														(format nil "Veuillez choisir entre : ~% 
																		~t 1- Définir un nouveau dinosaure ~% 
																		~t 2- Commencer le défi
																		~t 3- Quitter le programme" 
														) 
														"" :~OK :~cancel
					)				)
	)
	(case decision
		(1 (definir_dinosaure))
		(2 (h_m))
		(3 (message-box "FIN" "Au revoir":topmost t) (return 'FIN))
	)
)
)

;#####################################  Fonction d'initialisation  <A compléter>
; Remarque importante: le nom des propriétés sont toujours entre | |; ceci dû à un problème de conversion string<-->symbol
(defun init ()
(setf (get '|xx| 'type) 'bool)
(setf (get '|xx| 'description) "Description de xx")
(setf (get '|xx| 'question) "Question de xx")

(setf (get '|yy| 'type) 'string)
(setf (get '|yy| 'valeur) '("ho" "he" "ha") )
(setf (get '|yy| 'description) "Description de yy")
(setf (get '|yy| 'question) "Question de yy")

(setf (get '|zz| 'type) 'numerique)
(setf (get '|zz| 'valeur) '(0 100000) )
(setf (get '|zz| 'description) "Description de zz")
(setf (get '|zz| 'question) "Question de zz")
(defparameter *proprietes* '(|xx| |yy| |zz|))

(defparameter *dinosaures* '(|aa| |bb| |cc|))
(defparameter |aa| '( (|yy| exacte "ho" 1) (|zz| approx (1 10) 5) ) )
(defparameter |bb| '( (|xx| t 10) (|yy| exacte "ho" 1)  ) )
(defparameter |cc| '( (|xx| nil 5) (|zz| approx (1 20) 10) ) )

(defparameter *regles* '( ((|xx| t) (|zz| exacte 5)) ) )
)


;######################################  L'Homme choisi, la machine deviene!!

;-------------------------------------- Fonction main de cette partie
; Remarque importante: l'étape d'initialisation des arguments pour la fonction <sysyeme_expert> est primordial à comprendre. Il s'agit de 
; triplet (nom_dinosaure, réponses_correctes_fourinies_par_utilisateur,  évaluation_des_réponses_fourinies)
(defun h_m (&aux decision reponse msg)
(message-box (format nil "Choisissez un dinosaure parmi: ~{~& ~A ~}" *dinosaures*) "Dinosaures" :topmost t)
(message-box "Je vous poserai quelques questions; veuillez répondre de manière précise!" "Attention" :topmost t)
(setq msg 
(format nil 
"Vous avez trois types de réponses:~% 
	1- boolean ~%
	2- chaîne de caractères ~%
	3- numérique ~% ~%
	Pour le type 1; les réponses seront soit t soit nil ~%
	Pour le type 2 et 3, vous choisissez de donner une reponse de type EXACTE ou de type APPROX ~%
	~5T 1- pour le type 2; la réponse de type APPROX sera une succession de réponses possibles ~%
	~5T 2- pour le type 3; la réponse de type APPROX sera un intervalle dont vous précesera les bornes"
))
(message-box 	msg "Instructions" :topmost t)
(system_expert 
			(loop for dino in *dinosaures* collecting
				(list dino nil 0)
			)
			nil
)

(setq decision (parse-integer	(ask-user-for-string
														(format nil "Voulez-vous choisir d'autre dinosaure? : ~% 
																		~t 1- Oui ~% 
																		~t 2- non" (car (last reponse))
														) 
														"" :~OK :~cancel
				)				)
)
(when (= decision 1) (h_m))
)

;--------------------------------------------- Fonction qui utilise l'intelligence artificielle via la fonction de service <supprim>
;On considère qu'on devrait choisir un dinosaure parmi ceux qui restent (à savoir par éliminé) si il ne reste qu'un seul (ça y est; on l'a trouvé)
; ou si il n'y plus des questions à poser
; on essaie de donner un feed_back à l'utilisateur via la fonction <corriger> 
(defun system_expert (dinos reponses &aux (end t))
(when (> (length dinos) 1)
	(setq end nil)
	(let (rep prop)
		(setq prop (choisir_propriete reponses dinos))
		(if prop
			(progn
				(message-box (format nil (get prop 'question)) "Question" :topmost t)
				(setq rep (cons prop (ajouter_valeur prop)))
				(system_expert (supprim rep dinos) (cons rep reponses))
			)
			(setq end t)
		)
		
	)
)	
(when end
	(let (decision faute D  )
		(setq decision (parse-integer	(ask-user-for-string
																(format nil "  est-ce que votre dinosaure est ~a? : ~% 
																				~t 1- Oui ~% 
																				~t 2- non" (caar (last dinos))
																) 
																"" :~OK :~cancel
						)				)
		)
		(when (= decision 2) 
			(setq D (intern (ask-user-for-string "Entrez le nom de votre dinosaure" "" :~OK :~cancel ))	)	
			(setq faute (corriger D reponses))
			(if faute
				(message-box (format nil "Vous avez donné la réponse ~A " faute) "Attention" :topmost t)
				(message-box "Nous veillons à corriger notre base de règles" "System down" :topmost t)
			)
		)
	)
)
)

;-------------------------------------------------- On choisit une propriété dont on n'a pas encore posé la question
(defun choisir_propriete (reponses dinos &aux prop)
(loop with OK = 0 for D in dinos do
	(loop for D_prop in (symbol-value (car D)) do
		(unless (assoc (car D_prop) reponses) (setq OK 1) (setq prop (car D_prop)))
	)
	(when (= OK 1) (return prop))
)
)

;-------------------------------------------- Cette retourne la réponse aberrante qui à cause de lui on n'a pas réussi à trouver le bon dinoasaure 
(defun corriger (dino reponses)
(loop for R in reponses do
	(unless (supprim R `(,(list dino nil 0)) ) 
		(return R)
	)
)
)

;###################################################  Le coeur de notre programme
; ---------------------------------------- Fonction main de cette partie
; L'utilisateur donne une réponse par rapport à une propriété donnée. Il y a trois possibilités:
	; 1- Le dinosaure vérifie cette réponse, donc on garde le dinosaure et on lui donne un crédit qui varie d'une propriété à l'autre
	; 2- Le dinosaure ne vérifie pas cette réponse, donc on le supprime de notre liste
	; 3- On ne sait pas si le dinosaure vérifie la réponse par manque d'information pas example, 
		;alors on vérifie notre base de règles et si rien n'a été déduit on le garde on ne lui donne aucun crédit
(defun supprim (reponse dinos) 
(loop with New_dinos and OK for D in dinos do
	(setq OK (verif_prop reponse (symbol-value (car D))))	
	(case OK
		(1 	
			(setq New_dinos (my_sort 
									`(,(car D) ,(cons reponse (cadr D)) ,(+ (caddr D) (car (last (assoc (car reponse) (symbol-value (car D)))))))
									New_dinos
								)
			)
		)
		(2 )
		(3 
			(unless (verif_regles `(,reponse) (symbol-value (car D)))
				(setq New_dinos (my_sort D New_dinos))
			)
		)
	)
	finally
		(return New_dinos)
)
)

; ------------------------------------------- Cette fonction retourne vrai si on arrive à démontrer que le but est faux, sinon (si le but est
; vrai ou uncertain toujours par manque d'information) on retourne nil
(defun verif_regles (but base_faits &aux OK)
(loop for B in but do
	(setq OK nil)
	(case  (verif_prop B base_faits)
		(1 (setq OK t))
		(2 (setq OK nil))
		(3
			(loop for R in *regles* do
			(when (equal (caar R) (car B))
				(when (= (verif_prop B `(,(car R))) 1)
					(when (verif_regles (cdr R) base_faits) 
						(setq OK t)
						(return)
					)
				)
			)	
			)
		)
	)
	(unless OK (return))
)
OK
)

;---------------------------------------------- Cette fonction tri les dinosaures par ordre croissant par rapport à leur évaluation
(defun my_sort (X L)
(if L 
	(if (> (caddr X) (caddar L))
		(cons (car L) (my_sort X (cdr L)))
		(cons X L)
	)
`(,X)
)
)

;--------------------------------------------- Cette fonction montre la particuliarité d'un système expert d'ordre 0+. 
; Remaruqe importante: si le dinosaure ne contien pas l'information cherchée, on le considère comme uncertain 
(defun verif_prop (reponse dino)
(let ( (prop (assoc (car reponse ) dino)) )
	(if (and prop (not (or (equal (cadr reponse) 'xxx) (equal (cadr prop) 'xxx )) )    )
		(cond 
			((equal (get (car reponse) 'type) 'bool) (verif_prop_bool (cdr reponse) (cdr prop)))
			((equal (get (car reponse) 'type) 'string) (verif_prop_string (cdr reponse) (cdr prop)))
			((equal (get (car reponse) 'type) 'numerique) (verif_prop_num (cdr reponse) (cdr prop)))
		)
		3
	)
)
)

;--------------------------------------------- Aller jeter un coup d'oeil à la représentation de connaissances
(defun verif_prop_bool (reponse prop)
(if (and (equal (car reponse) (car prop))  )
	1
	2
)
)

;--------------------------------------------- Aller jeter un coup d'oeil à la représentation de connaissances
(defun verif_prop_num (reponse prop)
(let ( (type_rep (car reponse)) (type_prop (car prop)) (valeur_rep (cadr reponse)) (valeur_prop (cadr prop)) )
	(cond 
		( (and (equal type_rep 'exacte) (equal type_prop 'exacte) ) 
			(if (= valeur_rep valeur_prop)
				1
				2
			)
		)
		
		( (and (equal type_rep 'exacte) (equal type_prop 'approx) ) 
			(if (my_member2 valeur_rep valeur_prop)
				1
				2
			)
		)
		
		( (and (equal type_rep 'approx) (equal type_prop 'exacte) ) 
			(if (my_member2 valeur_prop valeur_rep )
				1
				2
			)
		)
		
		( (and (equal type_rep 'approx) (equal type_prop 'approx) ) 
			(if (my_intersection2 valeur_rep valeur_prop)
				1
				2
			)
		)
		
	)
)
)

;--------------------------------------------- Aller jeter un coup d'oeil à la représentation de connaissances
(defun verif_prop_string (reponse prop)
(let ( (type_rep (car reponse)) (type_prop (car prop)) (valeur_rep (cadr reponse)) (valeur_prop (cadr prop)) )
	(cond 
		( (and (equal type_rep 'exacte) (equal type_prop 'exacte) ) 
			(if (equal valeur_rep valeur_prop)
				1
				2
			)
		)
		
		( (and (equal type_rep 'exacte) (equal type_prop 'approx) ) 
			(if (my_member valeur_rep valeur_prop)
				1
				2
			)
		)
		
		( (and (equal type_rep 'approx) (equal type_prop 'exacte) ) 
			(if (my_member valeur_prop valeur_rep )
				1
				2
			)
		)
		
		( (and (equal type_rep 'approx) (equal type_prop 'approx) ) 
			(if (my_intersection valeur_rep valeur_prop)
				1
				2
			)
		)
		
	)
)
)

; ----------------------------------------------- member pour les chaîne de caractères
(defun my_member (X L)
(when L
	(if (equal X (car L))
		t
		(my_member X (cdr L))
	)
)
)

; ----------------------------------------------- member pour un intervalle. Exemple, (my_member 5 (1 10)) retourne vrai
(defun my_member2 (X L)
(and (>= X (car L)) (<= X (cadr L)))
)

; ----------------------------------------------- intersection  entre deux listes de chaîne de caractères
(defun my_intersection (L1 L2)
(when L1
	(if (my_member (car L1 ) L2)
		t
		(my_intersection (cdr L1) L2)
	)
)
)

; ----------------------------------------------- intersection  entre deux intervalles
(defun my_intersection2 (L1 L2)
(or (my_member2 (car L1) L2) (my_member2 (cadr L1) L2) )
)



;########################################################################################
;########################################################################################  Deuxième partie: définir un dinosaure
;########################################################################################


;*************************************************
;************************************************* 
;-------------------------------------------------- Fonction main de cette partie
; Pour définir un dinosaure, on a besoins d'en choisir un nom unique qui n'existe pas avant, lui donner de propriétés et vérfier que l'ensembles ; des ses propriétés caractérisent de manière unique ce nouveau dinosaure
(defun definir_dinosaure (&aux decision nom props)
(setq nom (nom_dinosaure))
(unless (eq nom 'cancel)
	(push nom *dinosaures*)
	(set nom props)
	(loop with D do
		(setq props (append props (ajouter_proprietes props)))
		(setq D (verification props))
		(if (equal D nom )
			(return)
			(message-box (format nil "Votre définition n'est pas complète; elle coincide avec la définition de ~A" D) "Attention" :topmost t)
		)
	)
)
)

;---------------------------------------------- Choisir un nom unique
(defun nom_dinosaure ()
(loop with nom and decision = 0 do
	(setq nom 	(intern (ask-user-for-string 	(format nil "~& Entrez le nom de dinosaure ")
										"" :~OK :~cancel
				)		)
	)
	(when (member nom *dinosaures*)
		(setq decision (parse-integer	(ask-user-for-string
														(format nil "Le dinosaure ~a existe déjà! ~% 
																	Entrez le numéro correspondant à votre choix: ~% 
																		~t 1- Réessayer ~% 
																		~t 2- Annuler la définition d'un nouveau dinosaure" nom
														) 
														"" :~OK :~cancel
						)				)
		)
	)
	(case decision
		(0 (return nom))
		(1 (setq decision 0))
		(2 (return 'cancel))
	)
) 
)
;-------------------------------------- On utilise la fonction <supprim> pour vérifier qu'il n'existe pas d'autre dinosaure dont la définition 
; coincide avec celle donnée par l'utilisateur.
(defun verification (props)
(let  (     (dinos (mapcar '(lambda (XX) (list XX nil 0)) *dinosaures*)) )
	(loop for R in props do
		(setq dinos (supprim R dinos ))
	)
	(caar (last dinos))
)
)

;**********************************************************
;********************************************************** 

;------------------------------- On ajoute une propriété soit en choisissant une existant soit en créant une nouvelle, puis on vérifie que
; l'ensemble des propriétés est cohérentes, à savoir il contredit pas notre base des règles
; Remarque importante: on ne permet pas de redéfinir une propriété, d'où les fonctions <exclure et supprim_element>
(defun ajouter_proprietes (propr &aux decision props)
(setq props (exclure propr *proprietes*))
(loop with proprietes and prop do
	(setq decision (parse-integer	(ask-user-for-string
															(format nil " Entrez le numéro correspondant à votre choix: ~% 
																			~t 1- Ajouter une nouvelle propriété ~% 
																			~t 2- Finir la définition" 
															) 
															"" :~OK :~cancel
							)				)
	)
	(case decision
		(2 (return proprietes))
		(1 
			(setq prop (ajouter_propriete props))
			(if (verif_regles (list prop) proprietes)
				(message-box "Votre propriété ne peut pas être accepté, elle contredit notre base de faits" "Attention" :topmost t)
				(progn 
					(push  prop proprietes)
					(setq props (supprim_element (car prop)  props))
				)
			)	
		)
		
	)
) 
)
;------------------------------------- Cette fonction retourne la liste L2 privé de L1
(defun exclure (L1 L2)
(when L2
	(if (my_member (car L2) L1)
		(exclure L1 (cdr L2))
		(cons (car L2) (exclure L1 (cdr L2)))
	)
)
)
;-------------------------------------- Cette fonction supprime un élément dans une liste
(defun supprim_element (X L)
(when L
	(if (equal X (car L))
		(cdr L)
		(cons (car L) (supprim_element X (cdr L)))
	)
)
)

;************************************************************
;************************************************************

;--------------------------------- Fonction main de cette partie 
; un triplet (nom_propriété, valeur, évaluation)
(defun ajouter_propriete (props &aux nom decision indice )
(setq indice (loop with i = 0 for X in props collecting `(,(setq i (+ i 1)) ,X)))
(setq decision 	(parse-integer 	(ask-user-for-string 
									(format nil "Entrez le numéro correspondant à votre choix ~%
										~:{~& ~a ~@A~}
										~&~a- Définir une nouvelle propriété" indice (+ 1 (caar (last indice)))
									)
									"" :~OK :~cancel
				)				)
)
(if (< (length props) decision)
	(setq nom (definir_propriete))
	(setq nom (element decision props))
)
(append 
	(cons nom (ajouter_valeur nom))
	`(,(parse-integer 	(ask-user-for-string 
									(format nil "Ese-ce que vous considérez cette propriété comme: ~%
										1- banal ~%
										5- caractérisant ~%
										10- très caractérisant " 
									)
									"" :~OK :~cancel
	)				))
)
)
;----------------------------------- Cette fonction retourne l'élément X-ème d'une liste
(defun element (X L)
(if (= X 1)
	(car L)
	(element (- X 1) (cdr L))
)
)

;*************************************************************
;*************************************************************

;------------------------------------------- La fonction main de cette partie
; Plus on donne liberté à l'utilisateur, plus difficile devient la programmation
; Trois possibilités:
	; 1- (bool xxx) ou (bool t) ou (bool nil) , xxx désigne qu'on ne sait pas la réponse
	; 2- (string xxx) ou (string exacte "fdfdfv") ou (string approx ("bhg" "bghfhb" "bhfgh")) , approx donne une liste de type ou
	; 3- (numerique xxx) ou (numerique exacte 500) ou (numerique approx (500 700))	, approx donne un intervalle
(defun ajouter_valeur (prop)
(message-box (format nil "Voici la description de ~a: ~% ~a" prop (get prop 'description)) "Descrition" :topmost t)
(let ( (type (get prop 'type)) (valeur (get prop 'valeur)) )
	(cond
		((eq type 'bool) (ajouter_valeur_bool))
		((eq type 'string) (ajouter_valeur_string valeur))
		((eq type 'numerique) (ajouter_valeur_num valeur))
	)
)
)
;-------------------------------------------------------
(defun ajouter_valeur_bool (&aux decision)
(setq decision (parse-integer	(ask-user-for-string
												(format nil " Entrez le numéro correspondant à votre choix: ~% 
																~t 1- Oui ~% 
																~t 2-  Non ~%
																~t 3- Ne pas savoir "
												) 
												"" :~OK :~cancel
				)				)
)
(case decision
	(1 (list t))
	(2 (list nil))
	(3 (list 'xxx))
)
)

;-------------------------------------------------------
(defun ajouter_valeur_num (valeur &aux decision)
	(setq decision 	(parse-integer	(ask-user-for-string
													(format nil " Entrez le numéro correspondant à votre choix: ~% 
														~t 1- Vous avez une valeur exacte à donner  ~% 
														~t 2-  Vous avez des valeurs approximatives à donner ~%
														~t 3- Ne pas savoir "
													) 
													"" :~OK :~cancel
					)				)
	)

(case decision
	(1 `(exacte ,(ajouter_valeur_num_exacte valeur)))
	(2 (list 'approx (ajouter_valeur_num_approx valeur)))
	(3 (list 'xxx))
)
)

;-------------------------------------------------------
(defun ajouter_valeur_num_exacte (valeur)
(if (or (> (car valeur ) 0) (< (cadr valeur) 100000))
	(loop with msg and reponse do
		(cond
			( (= (car valeur) 0) (setq msg (format nil "Vous devriez entrer une valeur inférieure à ~a :" (cadr valeur))))
			( (= (car valeur) 100000) (setq msg (format nil "Vous devriez entrer une valeur supérieure à ~a :" (car valeur))))
			(t (setq msg (format nil "Vous devriez entrer une valeur entre ~a et ~a  :" (car valeur) (cadr valeur))))
		)
		
		(setq reponse (parse-integer (ask-user-for-string	(format nil msg) "" :~OK :~cancel))	)
		(when (and (>= reponse (car valeur )) (<= reponse (cadr valeur ))) 
			(return reponse)
		)
	)
	(parse-integer (ask-user-for-string  "Veuillez entrez votre valeur" "" :~OK :~cancel))
)
)

;-------------------------------------------------------
(defun ajouter_valeur_num_approx (valeur &aux (b_inf 0) (b_sup 100000)  )
(if (or (> (car valeur ) 0) (< (cadr valeur) 100000))
	(loop with msg1 and msg2 do
		(when (or (= (car valeur) 0) (< (cadr valeur) 100000)) 
			(setq msg1 (format nil "Veuillez entrer la borne supérieure qui devrait être inférieure à ~a:" (cadr valeur)))
		)
		(when (or (= (cadr valeur) 100000) (> (car valeur) 0) ) 
			(setq msg2 (format nil "Veuillez entrer la borne inférieure qui devrait être supérieure à ~a:"(car valeur)))		
		)
		(print msg2)
		(when msg1 
			(setq b_sup (parse-integer (ask-user-for-string	(format nil msg1) "" :~OK :~cancel))	)
		)
		(when msg2 
			(setq b_inf (parse-integer (ask-user-for-string	(format nil msg2) "" :~OK :~cancel))	)
		)
		(when (and (>= b_inf (car valeur )) (<= b_sup (cadr valeur ))) 
			(return (list b_inf b_sup))
		)
	)
	`(,(parse-integer (ask-user-for-string  "Veuillez entrer la borne inférieure" "" :~OK :~cancel))
		,(parse-integer (ask-user-for-string  "Veuillez entrer la borne supérieure" "" :~OK :~cancel))
	)
)
)

;-------------------------------------------------------
(defun ajouter_valeur_string (valeur &aux decision)
(setq decision 	(parse-integer	(ask-user-for-string
													(format nil " Entrez le numéro correspondant à votre choix: ~% 
														~t 1- Vous avez une valeur exacte à donner  ~% 
														~t 2-  Vous avez des valeurs approximatives à donner 
														~t 3- Ne pas savoir"
													) 
													"" :~OK :~cancel
				)				)
)

(case decision
	(1 `(exacte ,(ajouter_valeur_string_exacte valeur)))
	(2 `(approx ,(ajouter_valeur_string_approx valeur)))
	(3 (list 'xxx))
)
)
;-------------------------------------------------------
(defun ajouter_valeur_string_exacte (valeur)
(if valeur
	(loop with msg and reponse do
		(setq msg (format nil "Vous devriez choisir parmi la liste suivante: ~%"))
		(setq reponse (ask-user-for-string	(concatenate 'string msg (format nil " ~{~& ~@A~}" valeur)) "" :~OK :~cancel))	
		(when (my_member reponse valeur) 
			(return reponse)
		)
	)
	(ask-user-for-string  "Veuillez entrez votre valeur" "" :~OK :~cancel)
)
)
;-------------------------------------------------------
(defun ajouter_valeur_string_approx (valeur &aux liste)
(if valeur
	(loop with msg and reponse and decision = 1 do
		(setq msg (format nil "Vous devriez choisir parmi la liste suivante: ~%"))
		(setq reponse (ask-user-for-string	(concatenate 'string msg (format nil " ~{~& ~@A~}" valeur)) "" :~OK :~cancel))	
		(when (my_member reponse valeur) 
			(push reponse liste)
			(setq decision (parse-integer	(ask-user-for-string
												(format nil " Entrez le numéro correspondant à votre choix: ~% 
																~t 1- Ajouter une nouvelle valeur~% 
																~t 2-  Finir la définition "
												) 
												"" :~OK :~cancel
							)				)
			)
		)
		(when (= decision 2) (return liste))
	)
	(loop with decision = 1 do
		(push (ask-user-for-string  "Veuillez entrez votre valeur" "" :~OK :~cancel) liste)
			(setq decision (parse-integer	(ask-user-for-string
												(format nil " Entrez le numéro correspondant à votre choix: ~% 
																~t 1- Ajouter une nouvelle valeur~% 
																~t 2-  Finir la définition "
												) 
												"" :~OK :~cancel
							)				)
			)
		(when (= decision 2) (return liste))
	)
)
)
;------------------------------------------------------- Rappel d'une fonction déjà définie avant
(defun my_member (X L)
(when L
	(if (equal X (car L))
		t
		(my_member X (cdr L))
	)
)
)


;***************************************************************
;***************************************************************

;------------------------------------------- Pour défnir une nouvelle propriété, on a besoin d'un nom unique, d'un type , d'une description et
; et d'une question qui sera posé à l'utilisateur
(defun definir_propriete (&aux prop)
(setq prop (nom_propriete))
(unless (eq prop 'cancel)
	(valeur_prop prop)
	(setf 	(get prop 'description) 
			(ask-user-for-string "Entrez une description de votre propriété" "" :~OK :~cancel)
	)
	(setf 	(get prop 'Question) 
			(ask-user-for-string "Entrez la question correspondant à votre propriété" "" :~OK :~cancel)
	)
	(push prop *proprietes*)
	prop
)
)

;------------------------------------------------------ nom unique
(defun nom_propriete ()
(loop with nom and decision = 0 do
	(setq nom 	(intern (ask-user-for-string 	(format nil "~& Entrez le nom de la nouvelle propriété ")
										"" :~OK :~cancel
				)		)
	)
	(when (member nom *proprietes*)
		(setq decision (parse-integer	(ask-user-for-string
														(format nil "La propriété ~a existe déjà! ~% 
																	Entrez le numéro correspondant à votre choix: ~% 
																		~t 1- Réessayer ~% 
																		~t 2- Annuler la définition d'une nouvelle propriété" nom
														) 
														"" :~OK :~cancel
						)				)
		)
	)
	(case decision
		(0 (return nom))
		(1 (setq decision 0))
		(2 (return 'cancel))
	)
) 
)

;------------------------------------------------------ valeur entre bool, string et numerique et puis choisit une certaine restriction pour 
; chaque type
(defun valeur_prop (nom &aux decision)
(setq decision (parse-integer	(ask-user-for-string
												(format nil " Entrez le numéro correspondant à votre choix: ~% 
																~t 1- Booléan ~% 
																~t 2- Chaîne de caractères ~% 
																~t 3- Valeur numérique "
												) 
												"" :~OK :~cancel
				)				)
)
(case decision
	(1 (prop_bool nom))
	(2 (prop_string nom))
	(3 (prop_num nom))
)
)
;------------------------------------------------------
(defun prop_bool (nom)
(setf (get nom 'type) 'bool)
)
;------------------------------------------------------
(defun prop_num (nom &aux decision)
(setf (get nom 'type) 'numerique)
(setq decision (parse-integer	(ask-user-for-string
												(format nil " Entrez le numéro correspondant à votre choix: ~% 
																~t 1- La valeur peut être n'importe quelle valeur numérique ~% 
																~t 2- La valeur ne peut prendre que parmi un intervalle"
												) 
												"" :~OK :~cancel
				)				)
)
(let ((borne_inf 0) (borne_sup 100000))
	(when (= decision 2)
		(setq borne_inf (parse-integer (ask-user-for-string
												(format nil "Entrez la borne inférieure de l'intervalle; 0 est la valeur la plus petite " )
												"" :~OK :~cancel
						)				)
		)
		(setq borne_sup (parse-integer (ask-user-for-string
												(format nil "Entrez la borne inférieure de l'intervalle; 100000 est la valeur la plus grande " )
												"" :~OK :~cancel
						)				)
		)
	)
	(setf (get nom 'valeur) `(,borne_inf ,borne_sup))
)
)
;------------------------------------------------------
(defun prop_string (nom &aux decision)
(setf (get nom 'type) 'string)
(setq decision (parse-integer	(ask-user-for-string
												(format nil " Entrez le numéro correspondant à votre choix: ~% 
																~t 1- La valeur peut être n'importe quelle chaîne de caractères ~% 
																~t 2- La valeur ne peut prendre que parmi une liste des valeurs"
												) 
												"" :~OK :~cancel
				)				)
)
(let (liste)
	(when (= decision 2)
		(loop with reponse and indice = 1 do
			(setq reponse (intern (ask-user-for-string
										(format nil " Entrez le ~a élément de la liste"indice) 
										"" :~OK :~cancel
			)				)		)
			(push reponse liste)
			(setq decision 	(parse-integer	(ask-user-for-string
												(format nil " Entrez le numéro correspondant à votre choix: ~% 
																~t 1- Ajouter une nouvel élément ~% 
																~t 2- Continuez la définition"
												) 
												"" :~OK :~cancel
							)				)
			)
			(case decision
				(1 	(setq indice (+ 1 indice)))
				(2 (return))
			)
		)
	)
	(setf (get nom 'valeur) liste)
)
)





