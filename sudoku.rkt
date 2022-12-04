#lang racket

; checks if row has symbols only from 0 to 9 (0 is equal to empty cell)
(define (checkSymbols row)
  (cond
    [(empty? row) #t]
    [(<= 0 (first row) 9) (checkSymbols (rest row))]
    [else #f]))

; checks that each rows' length is equal to 9
(define (checkBoardWidth board)
  (cond
    [(empty? board) #t]
    [(equal? (checkSymbols (car board)) #f) (println "One of numbers is out of 0-9 range.")] ; checking row symbols
    [(equal? (length (car board)) 9) (checkBoardWidth (rest board))] ; checking row size
    [else (println "One of rows does not have 9 elements")]))

; checks that each columns' length is equal to 9
(define (checkBoardHeight board)
  (if (equal? (length board) 9) ; we don't need to check each column because we'll check that each row contains 9 elements
      #t
      (println "Sudoku contains number of rows that is not equal to 9"))
  )

; checks user input (dimensions and symbols)
(define (checkInput board)
    (if (and (equal? (checkBoardHeight board) #t) (equal? (checkBoardWidth board) #t))
      #t
      #f))

; shows final result
(define (draw board)
  (cond
    [(empty? board) exit]
    [(println (car board)) (draw (rest board))]) ; printing solution line by line
  (exit)) ; stopping if we found one

 ; get particular cell
(define (getCellIndexes value)
  (cond
    [(< value 3) '(0 1 2)]
    [(> value 5) '(6 7 8)]
    [else '(3 4 5)]
    )
  )

; getting values in 3x3 cell 
(define (check3x3 board row column)
   ; creating a list of values
   (for*/list ((i (getCellIndexes row))(j (getCellIndexes column)))
      (list-ref (list-ref board i) j)))

; checks if we can place value to current cell
(define (checkCell brd row column cellValue)
  ; checks if current value already exists in this row
  (define (checkRow row column)
    (if (equal? (member cellValue (list-ref brd row)) #f)
        #t
        #f))
  
  ; checks if current value already exists in this column
  (define (checkColumn row column)
    (if (equal? (member cellValue (map (lambda(x)(list-ref x column)) brd)) #f)
        #t
        #f))
  
  ; checks if current value already exists in this 3x3 cell
  (define (check3x3Cell row column)
    (if (equal? (member cellValue (check3x3 brd row column)) #f)
    #t
    #f))
  
  ; if value is not in a row, in a column and in a 3x3 -> place it
  (if (and (equal? (checkRow row column) #t) (equal? (checkColumn row column) #t) (equal? (check3x3Cell row column) #t))
      #t
      #f))

; solves sudoku using backtracking
(define (solve board)
  (define (loop (currBoard board) (row 0) (column 0))
    
    ; get value at position row, column
    (define (getCurrentValue brd row column) (list-ref (list-ref brd row) column))

    ; checking if sudoku had defined cell at the beggining
    (define (checkIfAlreadyDefined row column)
      (if (equal? (getCurrentValue board row column) 0) ; checks that input board had 0 in that cell
          #f
          #t))

    ; increments undefined cells
    (define (incrementCellValue brd row column)
      (let ((cellValue (getCurrentValue brd row column)))
        (if (equal? cellValue 9) ; if value at this cell was 9 -> unset it and go to previous cell
          (let ((newBoard (list-set brd row (list-set (list-ref brd row) column 0))))
            (cond [(equal? (goToPrevCell newBoard row column) #f) #f])     
            (if (equal? (goToNextCell newBoard row column) #f)
                #f
                (while-loop newBoard row column)))
          (let ((newBoard (list-set brd row (list-set (list-ref brd row) column (+ cellValue 1))))) ; if value is not 9 -> increment it
            (if (equal? (checkCell brd row column (+ 1 cellValue)) #t)
                #t
                (incrementCellValue newBoard row column))
            (cond
              [(and (equal? column 8) (equal? row 8)) (and (println "Sudoku is solved") (draw newBoard))]) ; if we have found the solution at the last cell   
            (if (equal? (goToNextCell newBoard row column) #f)
                #f
                (while-loop newBoard row column))))))

    ; going to previous undefined cell
    (define (goToPrevCell brd row column)
      (cond
        [(and (equal? row 0) (equal? column 0)) #f])
      (if (equal? column 0) ; if column == 0 -> go to previous row, else -> decrement column
          (if (equal? (checkIfAlreadyDefined (- row 1) 8) #t)
              (goToPrevCell brd (- row 1) 8)
              (incrementCellValue brd (- row 1) 8))
          (if (equal? (checkIfAlreadyDefined row (- column 1)) #t)
              (goToPrevCell brd row (- column 1))
              (incrementCellValue brd row (- column 1))))) 

    ; going to next cell 
    (define (goToNextCell brd row column)
      (cond
        [(and (equal? row 8) (equal? column 8)) #f]
        [(equal? column 8) (while-loop brd (+ 1 row) 0)] ; moving to the next line
        [else (while-loop brd row (+ 1 column))]))

    ; loop to solve sudoku
    (define (while-loop brd row column)
      (cond
        [(equal? (checkIfAlreadyDefined row column) #f)
         (cond [(equal? (incrementCellValue brd row column) #f) #f])]
        [(and (equal? row 8) (equal? column 8)) (and (println "Sudoku is solved") (draw brd))]
        [(equal? (goToNextCell brd row column) #f) #f]
        [else #t]))

    ; checks if input have duplicates in row/column/3x3
    (define (checkDuplicates r c)
      (let ((value (getCurrentValue board row column)))
        (let ((checkedBoard (list-set board row (list-set (list-ref board row) column 0)))) ; temporarily removing current value from board to use checkCell function
          (cond
            [(not (equal? value 0)) ; only non-zero values interest us
             (cond [(equal? (checkCell checkedBoard r c value) #f) (and (println "Duplicates have been found") #f)])]
            [else (if (and (equal? c 8) (equal? r 8))
                      #t
                      (if (equal? c 8)
                          (checkDuplicates (+ 1 r) 0)
                          (checkDuplicates r (+ 1 c))))]))))
      
    (if (equal? (checkDuplicates 0 0) #f)
        #f
        (cond
          [(equal? (while-loop board row column) #f) #f]
          [else #t])))
    
  (cond
    [(equal? (loop board) #f) (println "Sudoku can't be solved.")]))

; runs the program
(define (run)
  (println "Enter your board:")
  (let ((board (read))) ; reading board from user input
    (if (equal? (checkInput board) #t)
        (solve board) ; if board is correct -> solve it
        (println "Invalid input."))))
