(use http-client)
(use medea)
(use alist-lib)
(require-extension typed-records)

; helper for reloading code
(define l (lambda () (load "/app/hello.scm")))

(defstruct vpc 
	(id "${var.vpc_id}") 
	(subnets #()))

(defstruct ec2 type ami-id 
	(subnet-id "${var.subnet_id}") 
	(key "${var.key}"))

(defstruct sg ingress egress)

; map ec2
(define (ec2->tf ec2)
	`((ami . ,(ec2-ami-id ec2))
		(subnet_id . ,(ec2-subnet-id ec2))
		(source_dest_check . #f)
		(key . ,(ec2-key ec2))))

; mappings of structs to tf types and mappers
(define (struct->mapper elem)
	(cond
		((ec2? elem) `(aws_instance . ,ec2->tf))))

; tf boilerplate and struct mapping
(define (tf name elem)
	(let* ([sm (struct->mapper elem)]
				 [type (car sm)]
				 [f (cdr sm)])
		`((,type . 
						 ((,name . ,(f elem)))))))

; merge tf structures by type
(define (merge ll)
	(fold (lambda (elem res)
					(let* ([key (car elem)]
								 [val (cdr elem)]
								 [exist (alist-ref/default res key '())])
						(alist-update key (append val exist) res))) '() ll))

; merge all given structs into one tf structure
(define (generate . ll) (merge (foldl append '() ll)))

(define prod-vpc (make-vpc id: "vpc-11111" subnets: #("subnet-1111")))

(define (warp)
	(generate 
		(tf 'moop (make-ec2 type: "t2" ami-id: "wat"))  
		(tf 'foop (make-ec2 type: "t2" ami-id: "wat"))))

(define (jwarp) (print (json->string (warp))))

