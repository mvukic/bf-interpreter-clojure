(ns bf-interpreter.core
	(:gen-class))


	(def program-pointer (atom 0)) 	;; points at current character in program
	(def memory-pointer (atom 0))	;; points at urrent memory location
	(def memory (ref (byte-array 30)))	;; memory
	(def program (vector))				;; program
	(def brackets (atom {}))
	(def running (atom true))			;; will indicate if parsing should stop
	(def output (vector))				;; Vector of resulting bytes (chars)

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

	(defn program-pointer-value
		"Set program pointer to value."
		[value]
		(reset! program-pointer value)
	)

	(defn increase-memory
		"Increases memory at location memory-pointer
			memory[memory-pointer] += 1
		"
		[]
		(println "Memory before: " (get memory @memory-pointer))
		(def memory (update-in memory [@memory-pointer] inc))
		(println "Memory after: " (get memory @memory-pointer))
	)
	(defn decrease-memory
		"Decrease memory at location memory-pointer
			memory[memory-pointer] -= 1
		"
		[]
		(println "Memory before: " (get memory @memory-pointer))
		(def memory (update-in memory [@memory-pointer] dec))
		(println "Memory after: " (get memory @memory-pointer))
	)

	(defn read-file
		"Opens file and reads chars to vector."
		[filename]
		(try
			(with-open [reader (clojure.java.io/reader filename)]
				(loop [c (.read reader)] 
				(if (not= c -1)
					(do 
						(def program (conj program (char c))) ;; save chars as actual chars
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
				(let [ end-pos (atom index) counter (atom 1)]
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
				(swap! brackets assoc index @end-pos)
				(swap! brackets assoc @end-pos index)
				)
			)
		)
	)
	
	(defn real-parse
		"Real parsing happens here."
		[]
		(while (= running true)
			(case (get program @program-pointer)
				\> (do
					(println "pomak ljevo")
					(memory-pointer-inc)
					(program-pointer-inc)
				)
				\< (do
					(println "pomak desno")
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
					(concat output [(get memory @memory-pointer)])
					(println (get memory @memory-pointer))
					(program-pointer-inc)
				)
				\, (println "Implement reading from stdin")
				\[ (do
					(if (= (get memory @memory-pointer) 0)
						(program-pointer-value (brackets @program-pointer)))
					(program-pointer-inc)
				)
				\] (if (not= (get memory @memory-pointer) 0)
					(program-pointer-value (brackets @program-pointer))
					(program-pointer-inc)
				)
			)
			(if (>= @program-pointer (count program))
				(swap! running false)
			)
			
		)

	)

	(defn print-result 
		"Prints resulting memory."
		[]
		(println output)
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
		(exit))
	)