;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname adventure) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require "adventure-define-struct.rkt")
(require "macros.rkt")
(require "utilities.rkt")

;;;
;;; OBJECT
;;; Base type for all in-game objects
;;;

(define-struct object
  ;; adjectives: (listof string)
  ;; List of adjectives to be printed in the description of this object
  (adjectives)
  
  #:methods
  ;; noun: object -> string
  ;; Returns the noun to use to describe this object.
  (define (noun o)
    (type-name-string o))

  ;; description-word-list: object -> (listof string)
  ;; The description of the object as a list of individual
  ;; words, e.g. '("a" "red" "door").
  (define (description-word-list o)
    (add-a-or-an (append (object-adjectives o)
                         (list (noun o)))))
  ;; description: object -> string
  ;; Generates a description of the object as a noun phrase, e.g. "a red door".
  (define (description o)
    (words->string (description-word-list o)))
  
  ;; print-description: object -> void
  ;; EFFECT: Prints the description of the object.
  (define (print-description o)
    (begin (printf (description o))
           (newline)
           (void))))

;;;
;;; CONTAINER
;;; Base type for all game objects that can hold things
;;;

(define-struct (container object)
  ;; contents: (listof thing)
  ;; List of things presently in this container
  (contents)
  
  #:methods
  ;; container-accessible-contents: container -> (listof thing)
  ;; Returns the objects from the container that would be accessible to the player.
  ;; By default, this is all the objects.  But if you want to implement locked boxes,
  ;; rooms without light, etc., you can redefine this to withhold the contents under
  ;; whatever conditions you like.
  (define (container-accessible-contents c)
    (container-contents c))
  
  ;; prepare-to-remove!: container thing -> void
  ;; Called by move when preparing to move thing out of
  ;; this container.  Normally, this does nothing, but
  ;; if you want to prevent the object from being moved,
  ;; you can throw an exception here.
  (define (prepare-to-remove! container thing)
    (void))
  
  ;; prepare-to-add!: container thing -> void
  ;; Called by move when preparing to move thing into
  ;; this container.  Normally, this does nothing, but
  ;; if you want to prevent the object from being moved,
  ;; you can throw an exception here.
  (define (prepare-to-add! container thing)
    (void))
  
  ;; remove!: container thing -> void
  ;; EFFECT: removes the thing from the container
  (define (remove! container thing)
    (set-container-contents! container
                             (remove thing
                                     (container-contents container))))
  
  ;; add!: container thing -> void
  ;; EFFECT: adds the thing to the container.  Does not update the thing's location.
  (define (add! container thing)
    (set-container-contents! container
                             (cons thing
                                   (container-contents container))))

  ;; describe-contents: container -> void
  ;; EFFECT: prints the contents of the container
  (define (describe-contents container)
    (begin (local [(define other-stuff (remove me (container-accessible-contents container)))]
             (if (empty? other-stuff)
                 (printf "There's nothing here.~%")
                 (begin (printf "You see:~%")
                        (for-each print-description other-stuff))))
           (void))))

;; move!: thing container -> void
;; Moves thing from its previous location to container.
;; EFFECT: updates location field of thing and contents
;; fields of both the new and old containers.
(define (move! thing new-container)
  (begin
    (prepare-to-remove! (thing-location thing)
                        thing)
    (prepare-to-add! new-container thing)
    (prepare-to-move! thing new-container)
    (remove! (thing-location thing)
             thing)
    (add! new-container thing)
    (set-thing-location! thing new-container)))

;; destroy!: thing -> void
;; EFFECT: removes thing from the game completely.
(define (destroy! thing)
  ; We just remove it from its current location
  ; without adding it anyplace else.
  (remove! (thing-location thing)
           thing))

;;;
;;; ROOM
;;; Base type for rooms and outdoor areas
;;;

(define-struct (room container)
  (hint is-final?)

  #:methods
  (define (description-word-list o)
    (cons "the" (object-adjectives o)))
  
  (define (print-description room)
    (begin (printf (words->string (object-adjectives room)))
           (newline)
           (void))))

;; new-room: string -> room
;; Makes a new room with the specified adjectives
(define (new-room adjectives room-hint is-final?)
  (make-room (string->words adjectives)
             '() room-hint is-final?))

;;;
;;; THING
;;; Base type for all physical objects that can be inside other objects such as rooms
;;;

(define-struct (thing container)
  ;; location: container
  ;; What room or other container this thing is presently located in.
  (location)
  
  #:methods
  (define (examine thing)
    (print-description thing))

  ;; prepare-to-move!: thing container -> void
  ;; Called by move when preparing to move thing into
  ;; container.  Normally, this does nothing, but
  ;; if you want to prevent the object from being moved,
  ;; you can throw an exception here.
  (define (prepare-to-move! container thing)
    (void)))

;; initialize-thing!: thing -> void
;; EFFECT: adds thing to its initial location
(define (initialize-thing! thing)
  (add! (thing-location thing)
        thing))

;; new-thing: string container -> thing
;; Makes a new thing with the specified adjectives, in the specified location,
;; and initializes it.
(define (new-thing adjectives location)
  (local [(define thing (make-thing (string->words adjectives)
                                    '() location))]
    (begin (initialize-thing! thing)
           thing)))

;;;
;;; DOOR
;;; A portal from one room to another
;;; To join two rooms, you need two door objects, one in each room
;;;

(define-struct (door thing)
  (;; destination: container
   ;; The place this door leads to
   destination

   ;; destination-name: string
   ;; The name of the destination that the door leads to
   destination-name

   ;; linked-door: door
   ;; The door that is linked
   linked-door
   
   ;; locked?: boolean
   ;; Whether or not the door is locked
   locked?

   ;; key: key
   ;; The name of the key that unlocks the door
   key
   )
  
  #:methods
  ;; go: door -> void
  ;; EFFECT: Moves the player to the door's location and (look)s around.
  (define (go door)
    (if (door-locked? door)
        (printf "The door is locked!")
        (if (and (room-is-final? (door-destination door)) (not (have? (the gold))))
            (printf "I need to grab my gold before I go the next room...")
            (begin (newline)
                   (move! me (door-destination door))
                   (look)))))

  (define (unlock door key)
    (when (= (key-id (door-key door)) (key-id key))
      (if (person-poisoned? me)
          (printf "You're arms are too weak from the poison to unlock the door! Find some antidote first.")
          (begin (set-door-locked?! door false)
                 (set-door-locked?! (door-linked-door door) false)
                 (printf (string-append (door-destination-name door) " has been unlocked!"))))))
  
  (define (print-description door)
    (begin (printf (string-append (words->string (add-a-or-an (filter (lambda (e) (not (empty? e))) (append (object-adjectives door) (list "door"))))) " to " (door-destination-name door)))
           (newline)
           (void))))

;; join: room string room string
;; EFFECT: makes a pair of doors with the specified adjectives
;; connecting the specified rooms.
(define (join! room1 adjectives1 destination-name1 room2 adjectives2 destination-name2 locked key)
  (local [(define r1->r2 (make-door (string->words adjectives1)
                                    '() room1 room2 destination-name2 '() locked key))
          (define r2->r1 (make-door (string->words adjectives2)
                                    '() room2 room1 destination-name1 '() locked key))]
    (begin (initialize-thing! r1->r2)
           (initialize-thing! r2->r1)
           (set-door-linked-door! r1->r2 r2->r1)
           (set-door-linked-door! r2->r1 r1->r2)
           (void))))

;;;
;;; PERSON
;;; A character in the game.  The player character is a person.
;;;

(define-struct (person thing)
  (;; strength?: boolean
   ;; Whether or not the person has consumed a strength potion
   strength?

   ;; alive?: boolean
   ;; Whether or not the person is alive
   alive?

   ;; poisoned?: boolean
   ;; Whether or not the person is poisoned
   poisoned?

   ;; walkthrough-mode?: boolean
   ;; Whether or not the current game is a walkthrough or not
   walkthrough-mode?
   ))

;; initialize-person: person -> void
;; EFFECT: do whatever initializations are necessary for persons.
(define (initialize-person! p)
  (initialize-thing! p))

;; new-person: string container -> person
;; Makes a new person object and initializes it.
(define (new-person adjectives location)
  (local [(define person
            (make-person (string->words adjectives)
                         '()
                         location false true false false))]
    (begin (initialize-person! person)
           person)))

;; This is the global variable that holds the person object representing
;; the player.  This gets reset by (start-game)
(define me empty)

;;;
;;; PROP
;;; A thing in the game that doesn't serve any purpose other than to be there.
;;;

(define-struct (prop thing)
  (;; noun-to-print: string
   ;; The user can set the noun to print in the description so it doesn't just say "prop"
   noun-to-print
   ;; examine-text: string
   ;; Text to print if the player examines this object
   examine-text
   )
  
  #:methods
  (define (noun prop)
    (prop-noun-to-print prop))

  (define (examine prop)
    (display-line (prop-examine-text prop))))

;; new-prop: string container -> prop
;; Makes a new prop with the specified description.
(define (new-prop description examine-text location)
  (local [(define words (string->words description))
          (define noun (last words))
          (define adjectives (drop-right words 1))
          (define prop (make-prop adjectives '() location noun examine-text))]
    (begin (initialize-thing! prop)
           prop)))

;;;
;;; ADD YOUR TYPES HERE!
;;;

(define-struct (box thing)
  (broken?)
  
  #:methods
  (define (open b)
    (set-box-broken?! b true))

  (define (container-accessible-contents box)
    (if (box-broken? box)
        (container-contents box)
        '())))

(define (new-box adjectives location)
  (local [(define the-box (make-box (string->words adjectives)
                                    '() location false))]
    (begin (initialize-thing! the-box)
           the-box)))

;; HAMMER
(define-struct (hammer thing) ()
  #:methods
  (define (swing hammer box)
    (begin (open box)
           (printf "The box has been broken! Check what's inside."))))

(define (new-hammer adjectives location)
  (local [(define the-hammer (make-hammer (string->words adjectives)
                                          '() location))]
    (begin (initialize-thing! the-hammer)
           the-hammer)))

;;PAPER
(define-struct (paper thing)
  (information)
  
  #:methods
  (define (read paper)
    (printf (string-append "The paper says: " (paper-information paper))))) 
  
(define (new-paper adjectives info location)
  (local [(define the-paper (make-paper (string->words adjectives)
                                        '() location info))]
    (begin (initialize-thing! the-paper)
           the-paper)))

;;LOCKER
(define-struct (locker thing)
  (unlocked? combination)
  
  #:methods
  (define (prepare-to-move! locker thing)
    (error "You can't move it! Unlock and get the key."))
  
  (define (lock locker)
    (set-locker-unlocked?! locker false))
  
  (define (unlock locker combo)
    (if (= combo (locker-combination locker))
        (begin (set-locker-unlocked?! locker true)
               (printf "The locker has been unlocked! Check what's inside."))
        (printf "Wrong combination. Read the paper more closely")))
  
  (define (container-accessible-contents locker)
    (if (locker-unlocked? locker)
        (container-contents locker)
        empty)))

(define (new-locker adjectives combination location)
  (local [(define the-locker (make-locker (string->words adjectives)
                                          '() location
                                          false
                                          combination))]
    (begin (initialize-thing! the-locker)
           the-locker)))

(define-struct (red-box box)
  (;; snake: snake
   ;; The snake that is inside of the box
   snake)

  #:methods
  (define (open b)
    (begin (set-box-broken?! b true)
           (bite (red-box-snake b) me)
           (printf "A green snake jumped out the box and bit you! You can't get to the next room while you're poisoned. Find the antidote!"))))

(define (new-red-box adjectives snake location)
  (local [(define the-red-box (make-red-box (string->words adjectives)
                                            '() location false snake))]
    (begin (initialize-thing! the-red-box)
           the-red-box)))

(define-struct (blue-box box) ()
  #:methods
  (define (open b)
    (begin (set-box-broken?! b true)
           (printf "You found antidote! Use it to get rid of snake poisoning."))))
        

(define (new-blue-box adjectives location)
  (local [(define the-blue-box (make-blue-box (string->words adjectives)
                                              '() location false))]
    (begin (initialize-thing! the-blue-box)
           the-blue-box)))

(define-struct (purple-box box) ()
  #:methods
  (define (open b)
    (begin (set-box-broken?! b true)           
           (printf "You found the key!"))))

(define (new-purple-box adjectives location)
  (local [(define the-purple-box (make-purple-box (string->words adjectives)
                                                  '() location false))]
    (begin (initialize-thing! the-purple-box)
           the-purple-box)))

(define-struct (snake thing) ()
  #:methods
  (define (bite snake person)
    (set-person-poisoned?! person true)))

(define (new-snake adjectives location)
  (local [(define the-snake (make-snake (string->words adjectives)
                                        '() location))]
    (begin (initialize-thing! the-snake)
           the-snake)))

(define-struct (antidote thing)
  (;; applied? boolean
   ;; Whether or not the player has applied the antidote
   applied?)

  #:methods
  (define (use antidote person)
    (if (antidote-applied? antidote)
        (printf "The bottle is empty, you used it already")
        (begin (if (not (person-poisoned? person))
                   (printf "The antidote can only be used a person who's been poisoned...")
                   (begin (set-antidote-applied?! antidote true)
                          (set-person-poisoned?! person false)
                          (printf "You got rid of the snake poisoning! Now, find the key and get to the next room.")))))))

(define (new-antidote adjectives location)
  (local [(define the-antidote (make-antidote (string->words adjectives)
                                              '() location false))]
    (begin (initialize-thing! the-antidote)
           the-antidote)))

(define-struct (chamber thing)
  (enter?)
  
  #:methods
  (define (pass-through c)
    (set-chamber-enter?! c true)))

(define (new-chamber adjectives location)
  (local [(define the-chamber (make-chamber (string->words adjectives)
                                            '() location noun))]
    (begin (initialize-thing! the-chamber)
           the-chamber)))

(define-struct (yellow-chamber chamber) ()
  #:methods
  (define (pass-through c)
    (begin (set-chamber-enter?! c true)
           (printf "The chamber was filled with toxic gas and you inhaled it. You died from its effects.")
           (newline)
           (kill (within c gas)))))

(define (new-yellow-chamber adjectives location)
  (local [(define the-yellow-chamber (make-yellow-chamber (string->words adjectives)
                                                          '() location noun))]
    (begin (initialize-thing! the-yellow-chamber)
           the-yellow-chamber)))

(define-struct (white-chamber chamber) ()
  #:methods
  (define (pass-through c)
    (begin (set-chamber-enter?! c true)
           (printf "There is chicken here. Eat it!"))))

(define (new-white-chamber adjectives location)
  (local [(define the-white-chamber (make-white-chamber (string->words adjectives)
                                                        '() location noun))]
    (begin (initialize-thing! the-white-chamber)
           the-white-chamber)))

(define-struct (orange-chamber chamber) ()
  #:methods
  (define (pass-through c)
    (begin (set-chamber-enter?! c true)
           (printf "You chose the right chamber! Take the key use it to get to the next room"))))

(define (new-orange-chamber adjectives location)
  (local [(define the-orange-chamber (make-orange-chamber (string->words adjectives)
                                                          '() location noun))]
    (begin (initialize-thing! the-orange-chamber)
           the-orange-chamber)))
 
;door1 poisonous chicken
(define-struct (chicken thing)
  (eaten?)
  
  #:methods
  (define (eat p)
    (begin (set-chicken-eaten?! p true)
           (printf "Turns out the chicken you ate was poisonous! You died from its effects.")
           (newline)
           (restart))))

(define (new-chicken adjectives location)
  (local [(define the-chicken (make-chicken (string->words adjectives)
                                            '() location noun))]
    (begin (initialize-thing! the-chicken)
           the-chicken)))
 
;door 2 poisonous gas
(define-struct (gas thing) ()
  #:methods
  (define (kill gas)
    (restart)))

(define (new-gas adjectives location)
  (local [(define the-gas (make-gas (string->words adjectives)
                                    '() location))]
    (begin (initialize-thing! the-gas)
           the-gas)))

(define-struct (fortune-teller person)
  (;; defeated?: boolean
   ;; Whether or not the player won Rock, Paper, Scissors
   defeated?)

  #:methods
  (define (play fortune-teller plr-choice)
    (if (fortune-teller-defeated? fortune-teller)
        (printf "You've already defeated the fortune teller!")
        (local [(define plr-choice-num (cond [(equal? (string-downcase plr-choice) "rock") 1]
                                             [(equal? (string-downcase plr-choice) "paper") 2]
                                             [(equal? (string-downcase plr-choice) "scissors") 3]
                                             [else (void)]))
                (define fortune-teller-choice (random 1 4))
                (define fortune-teller-choice-string (cond [(= fortune-teller-choice 1) "Rock"]
                                                           [(= fortune-teller-choice 2) "Paper"]
                                                           [(= fortune-teller-choice 3) "Scissors"]))]
          (begin (if (void? plr-choice-num)
                     (printf "Invalid choice. 'Rock', 'Paper', and 'Scissors' are your options.")
                     (begin (printf (string-append "You chose " plr-choice " and the fortune teller chose " fortune-teller-choice-string))
                            (newline)
                            (cond [(= plr-choice-num fortune-teller-choice) (printf "It's a draw!")]
                                  [(and (= plr-choice-num 1) (not (= fortune-teller-choice 3))) (printf "You lost!")]
                                  [(and (= plr-choice-num 2) (not (= fortune-teller-choice 1))) (printf "You lost!")]
                                  [(and (= plr-choice-num 3) (not (= fortune-teller-choice 2))) (printf "You lost!")]
                                  [else (begin (set-fortune-teller-defeated?! fortune-teller true)
                                               (printf "You won! Take the key to the next door from him! Hint: (take (within (the fortune-teller) key)), then (unlock (the next door) (within me key))"))])))))))

  (define (force-defeat fortune-teller)
    (begin (set-fortune-teller-defeated?! fortune-teller true)
           (printf "You beat the fortune teller in a game of Rock, Paper, Scissors!")))
  
  (define (container-accessible-contents f)
    (if (fortune-teller-defeated? f)
        (container-contents f)
        (begin (printf "Fortune Teller: 'Not so fast! You're gonna have to defeat me before you can see what I have in these pockets of mine!'")
               (newline)
               '()))))

(define (new-fortune-teller adjectives location)
  (local [(define fortune-teller (make-fortune-teller (string->words adjectives) '() location false true false false false))]
    (begin (initialize-thing! fortune-teller)
           fortune-teller)))

(define-struct (strength-potion prop)
  (;; consumed?: boolean
   ;; Whether or not the potion has been consumed
   consumed?)

  #:methods
  (define (drink strength-potion)
    (unless (strength-potion-consumed? strength-potion)
      (begin (set-person-strength?! me true)
             (set-strength-potion-consumed?! strength-potion true)
             (printf "You have obtained super strength!")))))

(define (new-strength-potion adjectives location)
  (local [(define adjectives-list (string->words adjectives))
          (define strength-potion (make-strength-potion adjectives-list '() location "strength-potion" "A potion that enhances a person strength once they drink it" false))]
    (begin (initialize-thing! strength-potion)
           strength-potion)))

(define-struct (key prop)
  (;; id: number
   ;; A unique identifier to correlate keys with their respective doors
   id)
  )

(define (new-key door-name key-id location)
  (local [(define adjectives (string->words door-name))
          (define the-key (make-key adjectives '() location "key" (string-append "The key that opens " door-name)  key-id))]
    (begin (initialize-thing! the-key)
           the-key)))

(define-struct (sword thing)
  ()

  #:methods
  (define (slash sword thief)
    (if (have? sword)
        (begin (printf "You slash the thief with your sword, killing him.")
               (newline)
               (kill thief))
        (printf "I need to take the sword before I can use it..."))))

(define (new-sword adjectives location)
  (local [(define adjectives-list (string->words adjectives))
          (define sword (make-sword adjectives-list '() location))]
    (begin (initialize-thing! sword)
           sword)))

(define-struct (gold thing)
  (stuck?)

  #:methods
  (define (prepare-to-move! gold thing)
    (if (gold-stuck? gold)
        (if (person-strength? me)
            (set-gold-stuck?! gold false)
            (if (person-walkthrough-mode? me)
                (printf "The gold is to heavy to carry! Maybe the fortune teller has something that could help...")
                (error "The gold is to heavy to carry! Maybe the fortune teller has something that could help...")))
        (void))))

(define (new-gold adjectives location stuck?)
  (local [(define the-gold (make-gold (string->words adjectives)
                                      '() location
                                      stuck?))]
    (begin (initialize-thing! the-gold)
           the-gold)))

(define-struct (thief prop)
  (;; alive?: boolean
   ;; Whether or not the thief is alive
   alive?)
  
  #:methods
  (define (kill thief)
    (when (and (thief-alive? thief) (have? (the sword)))
      (begin (set-thief-alive?! thief false)
             (printf "You've defeated the thief and secured your gold!")
             (newline)
             (printf "You win!")))))

(define (new-thief examine-text location)
  (local [(define words (string->words "dangerous thief"))
          (define noun (last words))
          (define adjectives (drop-right words 1))
          (define thief (make-thief adjectives '() location noun examine-text true))]
    (begin (initialize-thing! thief)
           thief)))

(define-struct (TV thing)
  (on?)

  #:methods
  (define (watch TV)
    (printf "You are watching TV")))

(define (new-TV adjectives location on-or-off)
  (local [(define the-TV (make-TV (string->words adjectives)
                                  '() location
                                  on-or-off))]
    (begin (initialize-thing! the-TV)
           the-TV)))

    
(define-struct (remote prop)
  ()

  #:methods
  (define (turn-on remote TV)
    (begin (set! TV-on? true)
           (printf "You have turned the TV on.\n"))))

(define (new-remote adjectives location)
  (local [(define adjectives-list (string->words adjectives))
          (define remote (make-remote adjectives-list '() location "remote" "A remote used to turn on TVs"))]
    (begin (initialize-thing! remote)
           remote)))

;;;
;;; USER COMMANDS
;;;

(define (look)
  (begin (printf "You are in ~A.~%"
                 (description (here)))
         (printf (room-hint (here)))
         (newline)
         (newline)
         (describe-contents (here))
         (void)))

(define-user-command (look) "Prints what you can see in the room")

(define (inventory)
  (if (empty? (my-inventory))
      (printf "You don't have anything.~%")
      (begin (printf "You have:~%")
             (for-each print-description (my-inventory)))))

(define-user-command (inventory)
  "Prints the things you are carrying with you.")

(define-user-command (examine thing)
  "Takes a closer look at the thing")

(define-user-command (describe-contents container) "Prints all the objects inside of a container. This includes people.")

(define (take thing)
  (move! thing me))

(define-user-command (take thing)
  "Moves thing to your inventory")

(define (drop thing)
  (move! thing (here)))

(define-user-command (drop thing)
  "Removes thing from your inventory and places it in the room")

(define (put thing container)
  (move! thing container))

(define-user-command (put thing container)
  "Moves the thing from its current location and puts it in the container.")

(define (help)
  (for-each (λ (command-info)
              (begin (display (first command-info))
                     (newline)
                     (display (second command-info))
                     (newline)
                     (newline)))
            (all-user-commands)))

(define-user-command (help)
  "Displays this help information")

(define-user-command (go door)
  "Go through the door to its destination")

(define (check condition)
  (if condition
      (display-line "Check succeeded")
      (error "Check failed!!!")))

(define-user-command (check condition)
  "Throws an exception if condition is false.")

;;;
;;; ADD YOUR COMMANDS HERE!
;;;

(define-user-command (swing hammer box) "Breaks the selected box using the hammer")
(define-user-command (unlock door) "Unlocks the door if you have the correct key")
(define-user-command (unlock locker combo) "Unlocks the locker if the correct code is given")
(define-user-command (play fortune-teller) "Engages the fortune teller in a game of Rock, Paper, Scissors")
(define-user-command (drink strength-potion) "Consumes the strength potion")
(define-user-command (slash thief) "Kills any thieves")
(define-user-command (turn-on remote TV) "Turns on the TV using the remote")
(define-user-command (watch TV) "Lets you watch the television")
(define-user-command (eat thing) "Lets you eat food from the location you're in")
(define-user-command (pass-through chamber) "Lets you pass through the thing")
(define-user-command (open thing) "Lets you open the thing")
(define-user-command (read paper) "Prints out what is written on the paper")

;;;
;;; THE GAME WORLD - FILL ME IN
;;;

;; start-game: -> void
;; Recreate the player object and all the rooms and things.
(define (start-game)
  ;; Fill this in with the rooms you want
  (local [(define room-1 (new-room "Room 1" "'That door needs a key. I wonder where it is...'\n(Hint: Use the 'swing' and 'unlock' commands. Use the 'help' command to see what each command does.)" false))
          (define room-1-box (new-box "brown rigid" room-1))
          (define door-1-key (new-key "Door 1" 1 room-1-box))
          (define room-2 (new-room "Room 2" "'Hmm, the key is probably in that locker, but I need the code to open it...'\n(Hint: Use the 'read' and 'unlock' commands)" false))
          (define room-2-locker (new-locker "small leather" 059 room-2))
          (define door-2-key (new-key "Door 2" 2 room-2-locker))
          (define room-3 (new-room "Room 3" "'Really, boxes again?'\n(Hint: Use the 'open' command. You might need to use the 'use' command)" false))
          (define room-3-purple-box (new-purple-box "" room-3))
          (define room-3-red-box (new-red-box "" '() room-3))
          (define room-3-snake (new-snake "green poisonous" room-3-red-box))
          (define door-3-key (new-key "Door 3" 3 room-3-purple-box))
          (define room-4 (new-room "Room 4" "'Hmm, the key must be in one of these chambers. At least it's not a box this time...'\n(Hint: Use the 'pass-through' command)" false))
          (define room-4-orange-chamber (new-orange-chamber "" room-4))
          (define door-4-key (new-key "Door 4" 4 room-4-orange-chamber))
          (define room-5 (new-room "Room 5" "Fortune Teller: 'Looking for next key, huh? Beat me in a game of Rock, Paper, Scissors and I'll hand it over.'\n(Hint: Use the 'play' command. You can also use the 'describe-contents' command to check what the fortune-teller has.)" false))
          (define fortune-teller (new-fortune-teller "" room-5))
          (define door-5-key (new-key "Door 5" 5 fortune-teller))
          (define room-6 (new-room "Room 6" "'Finally, my treasure! And, a sword? What would I need that for...'\n(Hint: Use the 'take' command. You might also need something from the previous room...)" false))
          (define room-7 (new-room "Room 7" "Thief: 'Thats some shiny gold you got there... Hand it over and nobody gets hurt.'\n(Hint: Use the 'slash' command)" true))]
    
    (begin (set! me (new-person "" room-1))
           ;; Add join commands to connect your rooms with doors
           (join! room-1 "next" "Room 1" room-2 "previous" "Room 2" true door-1-key)
           (join! room-2 "next" "Room 2" room-3 "previous" "Room 3" true door-2-key)
           (join! room-3 "next" "Room 3" room-4 "previous" "Room 4" true door-3-key)
           (join! room-4 "next" "Room 4" room-5 "previous" "Room 5" true door-4-key)
           (join! room-5 "next" "Room 5" room-6 "previous" "Room 6" true door-5-key)
           (join! room-6 "next" "Room 6" room-7 "previous" "Room 7: The Finale" true (new-key "Door 6" 6 room-6))
           
           ;; Room 1
           (new-hammer "medium-sized" room-1)
           
           ;; Room 2
           (new-paper "shredded looking" "059" room-2)
           (new-TV "" room-2 false)
           (new-remote "" room-2)

           ;; Room 3
           (set-red-box-snake! room-3-red-box room-3-snake)
           (new-antidote "" (new-blue-box "" room-3))
           
           ;; Room 4
           (new-chicken "" (new-white-chamber "" room-4))
           (new-gas "toxic" (new-yellow-chamber "" room-4))

           ;; Room 5
           (new-strength-potion "" fortune-teller)
           
           ;; Room 6
           (new-sword "shining" room-6)
           (new-gold "" room-6 true)

           ;; Room 7
           (new-thief "A thief that wants to steal your gold" room-7)
           
           (check-containers!)
           (void))))

(define (restart)
  (begin (printf "RESTARTING...")
         (newline) (newline)
         (start-game)
         (look)))

;;;
;;; PUT YOUR WALKTHROUGHS HERE
;;;

(define-walkthrough win
  (begin (restart)
         (set-person-walkthrough-mode?! me true)

         ;; Room 1
         (swing (the hammer) (the box))
         (newline)
         (unlock (the next door) (within (the box) key))
         (newline)
         (go (the next door))

         ;; Room 2
         (read (the paper))
         (newline)
         (unlock (the locker) (string->number (paper-information (the paper))))
         (newline)
         (unlock (the next door) (within (the locker) key))
         (newline)
         (go (the next door))

         ;; Room 3
         (open (the red-box))
         (newline)
         (open (the blue-box))
         (newline)
         (use (within (the blue-box) antidote) me)
         (newline)
         (open (the purple-box))
         (newline)
         (unlock (the next door) (within (the purple-box) key))
         (newline)
         (go (the next door))

         ;; Room 4
         (pass-through (the orange-chamber))
         (newline)
         (unlock (the next door) (within (the orange-chamber) key))
         (newline)
         (go (the next door))

         ;; Room 5
         (force-defeat (the fortune-teller))
         (newline)
         (unlock (the next door) (within (the fortune-teller) key))
         (newline)
         (go (the next door))

         ;; Room 6
         (take (the sword))
         (take (the gold))
         (newline)
         (go (the previous door))
         (drink (within (the fortune-teller) strength-potion))
         (newline)
         (go (the next door))
         (take (the gold))
         (unlock (the next door) (the key))
         (newline)
         (go (the next door))

         ;; Room 7
         (slash (within me "sword") (the thief))
         (newline)
         
         (set-person-walkthrough-mode?! me false)))

;;;
;;; UTILITIES
;;;

;; here: -> container
;; The current room the player is in
(define (here)
  (thing-location me))

;; stuff-here: -> (listof thing)
;; All the stuff in the room the player is in
(define (stuff-here)
  (container-accessible-contents (here)))

;; stuff-here-except-me: -> (listof thing)
;; All the stuff in the room the player is in except the player.
(define (stuff-here-except-me)
  (remove me (stuff-here)))

;; my-inventory: -> (listof thing)
;; List of things in the player's pockets.
(define (my-inventory)
  (container-accessible-contents me))

;; accessible-objects -> (listof thing)
;; All the objects that should be searched by find and the.
(define (accessible-objects)
  (append (stuff-here-except-me)
          (my-inventory)))

;; have?: thing -> boolean
;; True if the thing is in the player's pocket.
(define (have? thing)
  (eq? (thing-location thing)
       me))

;; have-a?: predicate -> boolean
;; True if the player as something satisfying predicate in their pocket.
(define (have-a? predicate)
  (ormap predicate
         (container-accessible-contents me)))

;; find-the: (listof string) -> object
;; Returns the object from (accessible-objects)
;; whose name contains the specified words.
(define (find-the words)
  (find (λ (o)
          (andmap (λ (name) (is-a? o name))
                  words))
        (accessible-objects)))

;; find-within: container (listof string) -> object
;; Like find-the, but searches the contents of the container
;; whose name contains the specified words.
(define (find-within container words)
  (find (λ (o)
          (andmap (λ (name) (is-a? o name))
                  words))
        (container-accessible-contents container)))

;; find: (object->boolean) (listof thing) -> object
;; Search list for an object matching predicate.
(define (find predicate? list)
  (local [(define matches
            (filter predicate? list))]
    (case (length matches)
      [(0) (error "There's nothing like that here")]
      [(1) (first matches)]
      [else (error "Which one?")])))

;; everything: -> (listof container)
;; Returns all the objects reachable from the player in the game
;; world.  So if you create an object that's in a room the player
;; has no door to, it won't appear in this list.
(define (everything)
  (local [(define all-containers '())
          ; Add container, and then recursively add its contents
          ; and location and/or destination, as appropriate.
          (define (walk container)
            ; Ignore the container if its already in our list
            (unless (member container all-containers)
              (begin (set! all-containers
                           (cons container all-containers))
                     ; Add its contents
                     (for-each walk (container-contents container))
                     ; If it's a door, include its destination
                     (when (door? container)
                       (walk (door-destination container)))
                     ; If  it's a thing, include its location.
                     (when (thing? container)
                       (walk (thing-location container))))))]
    ; Start the recursion with the player
    (begin (walk me)
           all-containers)))

;; print-everything: -> void
;; Prints all the objects in the game.
(define (print-everything)
  (begin (display-line "All objects in the game:")
         (for-each print-description (everything))))

;; every: (container -> boolean) -> (listof container)
;; A list of all the objects from (everything) that satisfy
;; the predicate.
(define (every predicate?)
  (filter predicate? (everything)))

;; print-every: (container -> boolean) -> void
;; Prints all the objects satisfying predicate.
(define (print-every predicate?)
  (for-each print-description (every predicate?)))

;; check-containers: -> void
;; Throw an exception if there is an thing whose location and
;; container disagree with one another.
(define (check-containers!)
  (for-each (λ (container)
              (for-each (λ (thing)
                          (unless (eq? (thing-location thing)
                                       container)
                            (error (description container)
                                   " has "
                                   (description thing)
                                   " in its contents list but "
                                   (description thing)
                                   " has a different location.")))
                        (container-contents container)))
            (everything)))

;; is-a?: object word -> boolean
;; True if word appears in the description of the object
;; or is the name of one of its types
(define (is-a? obj word)
  (let* ((str (if (symbol? word)
                  (symbol->string word)
                  word))
         (probe (name->type-predicate str)))
    (if (eq? probe #f)
        (member str (description-word-list obj))
        (probe obj))))

;; display-line: object -> void
;; EFFECT: prints object using display, and then starts a new line.
(define (display-line what)
  (begin (display what)
         (newline)
         (void)))

;; words->string: (listof string) -> string
;; Converts a list of one-word strings into a single string,
;; e.g. '("a" "red" "door") -> "a red door"
(define (words->string word-list)
  (string-append (first word-list)
                 (apply string-append
                        (map (λ (word)
                               (string-append " " word))
                             (rest word-list)))))

;; string->words: string -> (listof string)
;; Converts a string containing words to a list of the individual
;; words.  Inverse of words->string.
(define (string->words string)
  (string-split string))

;; add-a-or-an: (listof string) -> (listof string)
;; Prefixes a list of words with "a" or "an", depending
;; on whether the first word in the list begins with a
;; vowel.
(define (add-a-or-an word-list)
  (local [(define first-word (first word-list))
          (define first-char (substring first-word 0 1))
          (define starts-with-vowel? (string-contains? first-char "aeiou"))]
    (cons (if starts-with-vowel?
              "an"
              "a")
          word-list)))

;;
;; The following calls are filling in blanks in the other files.
;; This is needed because this file is in a different langauge than
;; the others.
;;
(set-find-the! find-the)
(set-find-within! find-within)
(set-restart-game! (λ () (start-game)))
(define (game-print object)
  (cond [(void? object)
         (void)]
        [(object? object)
         (print-description object)]
        [else (write object)]))

(current-print game-print)
   
;;;
;;; Start it up
;;;

(start-game)
(printf "On a journey to recover your long lost treasure, you discover a dungeon. Your instincts tell you, 'It has to be here...'")
(newline)
(newline)
(look)

