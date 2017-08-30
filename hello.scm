(use http-client)
(use medea)
(use alist-lib)
(require-extension typed-records)

(define l (lambda () (load "/app/hello.scm")))

(defstruct vpc 
	(id "${var.vpc_id}") 
	(subnets #()))

(defstruct ec2 type ami-id 
	(subnet-id "${var.subnet_id}") 
	(key "${var.key}"))

(defstruct sg ingress egress)

(define (ec2->json name ec2)
	`((aws_instance .
		((,name . 
						((ami . ,(ec2-ami-id ec2))
						 (subnet_id . ,(ec2-subnet-id ec2))
						 (source_dest_check . #f)
						 (key . ,(ec2-key ec2))))))))

(define prod-vpc (make-vpc id: "vpc-11111" subnets: #("subnet-1111")))

(define (xxx ll)
	(fold (lambda (elem res)
					(let* ([key (car elem)]
								 [val (cdr elem)]
								 [exist (alist-ref/default res key '())])
						(alist-update key (append val exist) res))) '() ll))

(define (x) (xxx '((a . (h . 2)) (a . (a . 3)) (b . (x . 1)))))

(define (generate #!rest ll) (xxx (foldl append '() ll)))

(define (woop) 
	(generate
		(ec2->json 'master (make-ec2 type: "t2.medium" ami-id: "ami-111"))
		(ec2->json 'public (make-ec2 type: "t2.medium" ami-id: "ami-111"))
		(ec2->json 'private (make-ec2 type: "t2.large" ami-id: "ami-222"))))

(define (jwoop) (print (json->string (woop))))

(define (cluster key)
	(let* ((i (make-ec2 type: "t2.medium" ami-id: "ami-1111"))
				 (m (master i key)))
		(json->string (ec2->json 'master i))))
