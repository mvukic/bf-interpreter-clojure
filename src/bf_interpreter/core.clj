(ns bf-interpreter.core
	(:gen-class))


	(def program-pointer (atom 0)) 		;; points at current character in program
	(def memory-pointer (atom 0))		;; points at urrent memory location
	(def memory (byte-array 30))		;; memory
	(def program (vector))				;; program
	(def brackets {})					;; map of pairs of brackets for loops
	(def running (atom true))			;; will indicate if parsing should stop
	(def output (vector))				;; Vector of resulting bytes (chars)
	(def operators [ \[ \] \< \> \, \. \+ \- ]) ;; Vector of brrainfuck operators

	(defn memory-at-pointer
		"Get memory byte at memory-pointer"
		[]
		(get memory @memory-pointer)
	)

	(defn memory-pointer-inc
		"Increase memory pointer by 1."
		[]
		(swap! memory-pointer inc)
	)

	(defn memory-pointer-dec
		"Decrease memory pointer by 1."
		[]
		(swap! memory-pointer dec)
	)

	(defn program-pointer-inc
		"Increase program pointer by 1."
		[]
		(swap! program-pointer inc)
	)

	(defn program-pointer-dec
		"Decrease program pointer by 1."
		[]
		(swap! program-pointer dec)
	)

	(defn program-pointer-set-value
		"Set program pointer to value."
		[value]
		(reset! program-pointer value)
	)
	(defn memory-set-value
		"Sets memory location at memory-pointer to value."
		[value]
		(aset-byte memory @memory-pointer value)
	)

	(defn increase-memory
		"Increases memory at location memory-pointer
			memory[memory-pointer] += 1
		"
		[]
		(aset-byte memory @memory-pointer (+ (memory-at-pointer) 1))
	)
	(defn decrease-memory
		"Decrease memory at location memory-pointer
			memory[memory-pointer] -= 1
		"
		[]
		(aset-byte memory @memory-pointer (- (memory-at-pointer) 1))
	)

	(defn save-to-output
		"Saves char at memory pointer to output."
		[]
		(def output (conj output (memory-at-pointer)))
	)

	(defn filter-chars
		"Saves only operator characters."
		[ch]
		(let [c (char ch)]
			(if (some #(= c %) operators) 
				(def program (conj program c))
			)
		)
	)
	(defn read-file
		"Opens file and reads chars to vector."
		[filename]
		(try
			(with-open [reader (clojure.java.io/reader filename)]
				(loop [c (.read reader)] 
				(if (not= c -1)
					(do 
						(filter-chars c)
						(recur (.read reader))
					))
				)
			)
			(catch Exception e (do 
				(println "Caught exception: " (.getMessage e)) 
				(System/exit 0))
			)
		)
	)

	(defn find-brackets-pairs
		"Finds pairs of brackets."
		[]
		(doseq [[index item] (map-indexed vector program)]
			(if (= item \[) 
				(let [ 
					end-pos (atom index) 
					counter (atom 1)
				]
				(while (> @counter 0)
					(do
						(swap! end-pos inc)
						(if (= (get program @end-pos) \[)
							(swap! counter inc))
						(if (= (get program @end-pos) \])
							(swap! counter dec))
					)	
				)
				;; Save bracket pair
				;; Index of opening bracket is key
				;; Index of closing key is value
				;; And reverse
				(def brackets (assoc brackets index @end-pos))
				(def brackets (assoc brackets @end-pos index))
				)
			)
		)
	)
	
	(defn real-parse
		"Real parsing happens here."
		[]
		(while @running
			(let [current-oper (get program @program-pointer)]
			(case current-oper
				\> (do
					(memory-pointer-inc)
					(program-pointer-inc)
				)
				\< (do
					(memory-pointer-dec)
					(program-pointer-inc)
				)
				\+ (do
					(increase-memory)
					(program-pointer-inc)
				)
				\- (do
					(decrease-memory)
					(program-pointer-inc)
				)
				\. (do
					(save-to-output)
					(println (char (last output)))
					(program-pointer-inc)
				)
				\, (do
					(println "Enter value: ()")
					(let [ input (read-string (read-line))]
						(memory-set-value (first (seq (char-array (str input)))))
					)
					(program-pointer-inc)
				)
				\[ (do
					(if (= (memory-at-pointer) 0)
						(program-pointer-set-value (brackets @program-pointer)))
					(program-pointer-inc)
				)
				\] (if (not= (memory-at-pointer) 0)
					(program-pointer-set-value (brackets @program-pointer))
					(program-pointer-inc)
				)
				() ;; Ignore avery other letter.
			)
			(if (= @program-pointer (count program))
				(reset! running false)
			)
				
			)
		)
	)
	
	(defn byte-array-into-string
		"Turns byte array into string."
		[byte-array]
		(apply str (map #(char (bit-and % 255)) byte-array))
	)

	(defn print-result 
		"Prints resulting memory."
		[]
		(println "Memory: " (vec memory))
		(println "Output Vector: " (vec output))
		(println "Output String: " (byte-array-into-string output))
	)

	(defn parse
		"Loops through chars and does something."
		[]
		(find-brackets-pairs)
		(real-parse)
	)

	(defn exit
		"Wrong number of arguments. Requires path to file as first argument."
		[]
		(println "Wrong number of arguments!")
		(System/exit 1)
	)


	(defn -main
		"Main brainfuck interpreter function."
		[& args]
		(if (= 1 (count args))
			(do
				(read-file (first args))
				(parse)
				(print-result)
			) 
			(exit)
		)
	)