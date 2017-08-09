(ns bf-interpreter.core
	(:gen-class))


	(def program-pointer (ref 0)) 	;; points at current character in program
	(def memory-pointer (ref 0))	;; points at urrent memory location
	(def memory (ref (byte-array 30)))	;; memory
	(def program (vector))				;; program
	(def brackets (ref {}))

	(defn update-memory-pointer 
		([] (dosync (alter memory-pointer + 1)))
		([value] (dosync (ref-set memory-pointer value)))
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
		"Finds pairs of brackets"
		[]
		(let [
			end-pose 0
			counter 1
		]
			(for [item (map-indexed vector program)]
				(println "w")
			)
		)
	)
	
	(defn real-parse
		"Real parsing happens here."
		[]
		(println "real parsing")

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
		) 
		(exit))
	)