;;;; src/syntax/named-patterns.lisp --- The named patterns of the search patterns: addresses, card numbers, hashes, encoded blocks.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; [[:ipv4:]], [[:creditcard:]], [[:base64:]] and the others, each what a
;;;; standard says the thing is: a pattern written out here, or, for what a
;;;; pattern cannot check (a card number's Luhn digit, an address's checksum, a
;;;; block over lines), a filter the search runs as it matches, reading the
;;;; text on past its window when it must. The search (search.lisp) reads them.

(in-package #:tuatara.syntax)

;;; Named patterns: [[:ipv4:]], [[:creditcard:]] and the others, each what a
;;; standard says the thing is, written out by the functions below.

(defparameter *filter-mark* "tuatarazfilter"
  "How a named pattern marks, in the text cl-ppcre is given, where code reads
the text instead of a pattern: an empty named group, (?<tuatarazfiltercard>),
made the filter its name gives (*PATTERN-FILTERS*) once the pattern is read.")

(defun dec-octet ()
  "RFC 3986's dec-octet, 0 to 255 with no leading zero, as a pattern."
  "(?:25[0-5]|2[0-4][0-9]|1[0-9]{2}|[1-9][0-9]|[0-9])")

(defun ipv4-core ()
  "RFC 3986's IPv4address, four dec-octets apart by dots, with no edges."
  (format nil "(?:~a\\.){3}~a" (dec-octet) (dec-octet)))

(defun ipv4-pattern ()
  "An IPv4 address standing alone: no digit or dot just before it, and no
digit, or dot and digit, just after it, so that 1.2.3.4.5 and 1.2.3.456
hold none."
  ;; first, cheaply, a dot after the first number
  (format nil "(?<![0-9.])(?=[0-9]{1,3}\\.)~a(?![0-9]|\\.[0-9])" (ipv4-core)))

(defun ipv6-pattern ()
  "An IPv6 address as RFC 3986's IPv6address has it (RFC 4291's text forms):
eight groups of one to four hex digits, a run of them left out as ::, the
last two may be an IPv4 address; not inside a word (no letter, digit,
underscore or colon just before it, nor a letter, digit or underscore just
after, so std::cout holds none), nor followed by a colon and more of an
address, or a dot and a digit; a colon that ends a sentence may follow."
  (let* ((h16 "[0-9A-Fa-f]{1,4}")
         (ls32 (format nil "(?:~a|~a:~a)" (ipv4-core) h16 h16))
         (forms (list (format nil "(?:~a:){6}~a" h16 ls32)
                      (format nil "::(?:~a:){5}~a" h16 ls32)
                      (format nil "(?:~a)?::(?:~a:){4}~a" h16 h16 ls32)
                      (format nil "(?:(?:~a:){0,1}~a)?::(?:~a:){3}~a" h16 h16 h16 ls32)
                      (format nil "(?:(?:~a:){0,2}~a)?::(?:~a:){2}~a" h16 h16 h16 ls32)
                      (format nil "(?:(?:~a:){0,3}~a)?::~a:~a" h16 h16 h16 ls32)
                      (format nil "(?:(?:~a:){0,4}~a)?::~a" h16 h16 ls32)
                      (format nil "(?:(?:~a:){0,5}~a)?::~a" h16 h16 h16)
                      (format nil "(?:(?:~a:){0,6}~a)?::" h16 h16))))
    ;; first, cheaply, a colon within the first five characters, as every form has
    (format nil "(?<![0-9A-Za-z_:])(?=[0-9A-Fa-f]{0,4}:)(?:~{~a~^|~})(?![0-9A-Za-z_]|:[0-9A-Fa-f:]|\\.[0-9])" forms)))

(defun mac-pattern ()
  "An IEEE 802 MAC address (EUI-48): six pairs of hex digits apart by hyphens,
as IEEE writes them, or by colons; or three groups of four apart by dots."
  (let ((h2 "[0-9A-Fa-f]{2}") (h4 "[0-9A-Fa-f]{4}"))
    (format nil "(?<![0-9A-Fa-f:.-])(?:~a(?:-~a){5}|~a(?::~a){5}|~a\\.~a\\.~a)(?![0-9A-Fa-f]|[:.-][0-9A-Fa-f])"
            h2 h2 h2 h2 h4 h4 h4)))

(defun email-pattern ()
  "An e-mail address as the HTML standard's valid e-mail address has it
\(WHATWG): the name of letters, digits and .!#$%&'*+/=?^_`{|}~-, an @, and
labels of letters, digits and hyphens apart by dots, none starting or ending
with a hyphen nor longer than 63."
  (let ((name "[A-Za-z0-9.!#$%&'*+/=?^_`{|}~-]")
        (label "[A-Za-z0-9](?:[A-Za-z0-9-]{0,61}[A-Za-z0-9])?"))
    (format nil "(?<!~a)~a+@~a(?:\\.~a)*(?![A-Za-z0-9-]|\\.[A-Za-z0-9])" name name label label)))

(defun url-pattern ()
  "A web or FTP address, http, https, ftp or ftps (in any case), :// and the
characters RFC 3986 lets a URI hold; its end not a dot, comma, semicolon,
colon, mark, quote or bracket, which close a sentence rather than the address
\(RFC 3986, appendix C), but for a closing bracket whose opening one it holds:
https://en.wikipedia.org/wiki/Kea_(bird) whole, (see https://x.nz/a) without
the last bracket."
  (let* ((inside "[A-Za-z0-9._~:/?#\\[\\]@!$&'*+,;=%-]")
         (last "[A-Za-z0-9_~/#@$&*+=%-]")
         (brackets (format nil "\\(~a*\\)" inside)))
    (format nil "(?<![A-Za-z0-9+.-])(?:[Hh][Tt][Tt][Pp][Ss]?|[Ff][Tt][Pp][Ss]?)://(?:~a|~a)*(?:~a|~a)"
            inside brackets last brackets)))

(defun uuid-pattern ()
  "A UUID as RFC 9562 writes one: 8-4-4-4-12 hex digits, its version (1 to 8)
and variant (8, 9, a or b) where they stand; or the nil or max UUID."
  (let ((h "[0-9A-Fa-f]"))
    (format nil "(?<![0-9A-Fa-f])(?:~a{8}-~a{4}-[1-8]~a{3}-[89ABab]~a{3}-~a{12}|0{8}-0{4}-0{4}-0{4}-0{12}|[Ff]{8}-[Ff]{4}-[Ff]{4}-[Ff]{4}-[Ff]{12})(?![0-9A-Fa-f])"
            h h h h h)))

(defun guid-pattern ()
  "A GUID as Windows writes one: any 8-4-4-4-12 hex digits, in braces or not."
  (let ((core "[0-9A-Fa-f]{8}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{12}"))
    (format nil "(?:\\{~a\\}|(?<![0-9A-Fa-f{])~a(?![0-9A-Fa-f}]))" core core)))

(defun hex-run-pattern (digits)
  "Exactly DIGITS hex digits standing alone, as a digest is written."
  (format nil "(?<![0-9A-Fa-f])[0-9A-Fa-f]{~d}(?![0-9A-Fa-f])" digits))

(defparameter *card-numbers*
  '(("visa" "4" (13 16 19))
    ("mastercard" "5[1-5]|222[1-9]|22[3-9]|2[3-6]|27[01]|2720" (16))
    ("amex" "3[47]" (15))
    ("discover" "6011|64[4-9]|65|622(?:1(?:2[6-9]|[3-9])|[2-8]|9(?:[01]|2[0-5]))" (16 17 18 19))
    ("diners" "36|30[0-5]|3095|3[89]" (14 15 16 17 18 19))
    ("jcb" "35(?:2[89]|[3-8])" (16 17 18 19))
    ("unionpay" "62" (16 17 18 19))
    ("maestro" "5018|5020|5038|5893|6304|6759|676[1-3]" (12 13 14 15 16 17 18 19))
    ("mir" "220[0-4]" (16 17 18 19)))
  "The payment card networks a named pattern finds the numbers of, each (NAME
PREFIXES LENGTHS): PREFIXES, a pattern for how its numbers begin, the
network's issuer numbers (ISO/IEC 7812) as it publishes them; LENGTHS, how
many digits its numbers have.")

(sb-ext:defglobal **card-prefixes** (make-hash-table :test 'equal :synchronized t)
  "The scanner of each network's PREFIXES (*CARD-NUMBERS*), by name.")

(defun card-network-p (digits length network)
  "True when DIGITS, a string of LENGTH digits, is a number of the card
NETWORK (an entry of *CARD-NUMBERS*): one of its lengths and prefixes."
  (destructuring-bind (name prefixes lengths) network
    (and (member length lengths)
         (cl-ppcre:scan (or (gethash name **card-prefixes**)
                            (setf (gethash name **card-prefixes**)
                                  (cl-ppcre:create-scanner (format nil "\\A(?:~a)" prefixes))))
                        digits))))

(defun luhn-p (digits)
  "True when DIGITS, a string of digits, passes the Luhn check (ISO/IEC
7812): every second digit from the last doubled, nine taken off a double past
nine, the sum a multiple of ten."
  (let ((sum 0))
    (loop for i from (1- (length digits)) downto 0
          for count from 0
          do (let ((digit (- (char-code (char digits i)) 48)))
               (when (oddp count)
                 (setf digit (* 2 digit))
                 (when (> digit 9) (decf digit 9)))
               (incf sum digit)))
    (zerop (mod sum 10))))

(defun card-groups-p (separators length)
  "True when SEPARATORS, the numbers of digits before each space or hyphen
of a card number of LENGTH digits, are how such a number is written: none;
one after every fourth digit; or, for 14 and 15 digits, after the fourth and
the tenth."
  (or (null separators)
      (equal separators (loop for k from 4 below length by 4 collect k))
      (and (member length '(14 15)) (equal separators '(4 10)))))

(defun card-end (text start end networks)
  "Where the card number that begins at START of TEXT (before END) ends, or
NIL when none does: the most digits, read with the spaces or hyphens between
them, that are written as a card number is (CARD-GROUPS-P), are a number of
one of NETWORKS (CARD-NETWORK-P), pass the Luhn check, and are followed by
no digit."
  (let ((digits (make-string 19)) (ends (make-array 20 :initial-element nil))
        (count 0) (separators '()) (i start))
    (loop while (and (< i end) (< count 19))
          do (let ((c (char text i)))
               (cond ((char<= #\0 c #\9)
                      (setf (char digits count) c)
                      (incf count) (incf i)
                      (setf (aref ends count) i))
                     ((and (plusp count) (find c " -") (< (1+ i) end) (char<= #\0 (char text (1+ i)) #\9))
                      (push count separators) (incf i))
                     (t (return)))))
    (setf separators (nreverse separators))
    (loop for length from count downto 12
          for after = (aref ends length)
          ;; no digit right after it: the next is a space or hyphen, or no digit
          when (and (or (< length count) (>= after end) (not (char<= #\0 (char text after) #\9)))
                    (or (= length count) (member length separators))
                    (card-groups-p (remove-if (lambda (k) (>= k length)) separators) length))
            do (let ((number (subseq digits 0 length)))
                 (when (and (some (lambda (network) (card-network-p number length network)) networks)
                            (luhn-p number))
                   (return after))))))

(defun card-filter (&optional network)
  "A cl-ppcre filter that matches a card number of NETWORK (a name of
*CARD-NUMBERS*), or of any network, where it stands (CARD-END)."
  (let ((networks (if network
                      (list (assoc network *card-numbers* :test #'string=))
                      *card-numbers*)))
    (lambda (position)
      (card-end cl-ppcre::*string* position cl-ppcre::*end-pos* networks))))

(defparameter *iban-lengths*
  '(("AD" . 24) ("AE" . 23) ("AL" . 28) ("AT" . 20) ("AZ" . 28) ("BA" . 20) ("BE" . 16) ("BG" . 22)
    ("BH" . 22) ("BI" . 27) ("BR" . 29) ("BY" . 28) ("CH" . 21) ("CR" . 22) ("CY" . 28) ("CZ" . 24)
    ("DE" . 22) ("DJ" . 27) ("DK" . 18) ("DO" . 28) ("EE" . 20) ("EG" . 29) ("ES" . 24) ("FI" . 18)
    ("FK" . 18) ("FO" . 18) ("FR" . 27) ("GB" . 22) ("GE" . 22) ("GI" . 23) ("GL" . 18) ("GR" . 27)
    ("GT" . 28) ("HR" . 21) ("HU" . 28) ("IE" . 22) ("IL" . 23) ("IQ" . 23) ("IS" . 26) ("IT" . 27)
    ("JO" . 30) ("KW" . 30) ("KZ" . 20) ("LB" . 28) ("LC" . 32) ("LI" . 21) ("LT" . 20) ("LU" . 20)
    ("LV" . 21) ("LY" . 25) ("MC" . 27) ("MD" . 24) ("ME" . 22) ("MK" . 19) ("MN" . 20) ("MR" . 27)
    ("MT" . 31) ("MU" . 30) ("NI" . 28) ("NL" . 18) ("NO" . 15) ("OM" . 23) ("PK" . 24) ("PL" . 28)
    ("PS" . 29) ("PT" . 25) ("QA" . 29) ("RO" . 24) ("RS" . 22) ("RU" . 33) ("SA" . 24) ("SC" . 31)
    ("SD" . 18) ("SE" . 24) ("SI" . 19) ("SK" . 24) ("SM" . 27) ("SO" . 23) ("ST" . 25) ("SV" . 28)
    ("TL" . 23) ("TN" . 24) ("TR" . 26) ("UA" . 29) ("VA" . 22) ("VG" . 24) ("XK" . 20) ("YE" . 30))
  "How many characters an IBAN of each country has, by its code, as the IBAN
registry (SWIFT, for ISO 13616) lists them.")

(declaim (inline alphanumericp*))
(defun alphanumericp* (c)
  "True when C is an ASCII letter or digit."
  (declare (type character c))
  (or (char<= #\0 c #\9) (char<= #\A c #\Z) (char<= #\a c #\z)))

(defun iban-check-p (iban)
  "True when IBAN, its characters without spaces, passes ISO 7064 MOD 97-10:
its first four put last, each letter written as its number (A 10 to Z 35),
the whole leaves 1 when divided by 97."
  (let ((rest 0))
    (flet ((add (c)
             (let ((value (digit-char-p c 36)))
               (setf rest (mod (if (< value 10) (+ (* rest 10) value) (+ (* rest 100) value)) 97)))))
      (loop for i from 4 below (length iban) do (add (char iban i)))
      (loop for i from 0 below 4 do (add (char iban i))))
    (= rest 1)))

(defun iban-end (text start end)
  "Where the IBAN that begins at START of TEXT (before END) ends, or NIL when
none does (ISO 13616): a country's code, two check digits and the rest, as
many characters in all as that country's IBANs have (*IBAN-LENGTHS*),
written together or in groups of four apart by a space, followed by no
letter or digit, passing the check (IBAN-CHECK-P). Letters are capitals, or
all small."
  (let* ((code (and (<= (+ start 2) end) (string-upcase (subseq text start (+ start 2)))))
         (length (cdr (assoc code *iban-lengths* :test #'equal))))
    (when length
      (let* ((spaced (and (< (+ start 4) end) (char= (char text (+ start 4)) #\Space)))
             (iban (make-string length)) (i start))
        (dotimes (k length)
          (when (and spaced (plusp k) (zerop (mod k 4)))
            (unless (and (< i end) (char= (char text i) #\Space)) (return-from iban-end nil))
            (incf i))
          (unless (and (< i end) (alphanumericp* (char text i))) (return-from iban-end nil))
          (setf (char iban k) (char text i))
          (incf i))
        (and (or (>= i end) (not (alphanumericp* (char text i))))
             (or (string= iban (string-upcase iban)) (string= iban (string-downcase iban)))
             (every #'digit-char-p (subseq iban 2 4))
             (iban-check-p iban)
             i)))))

(defparameter *sha256-constants*
  (coerce '(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
            #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
            #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
            #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
            #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85
            #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
            #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
            #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2)
          '(simple-array (unsigned-byte 32) (64)))
  "SHA-256's round constants (FIPS 180-4).")

(defun sha256 (octets)
  "The SHA-256 digest of OCTETS (FIPS 180-4), 32 octets: for the few octets
a check sums, as a Bitcoin address's."
  (declare (optimize speed))
  (let* ((k *sha256-constants*)
         (h (make-array 8 :element-type '(unsigned-byte 32)
                          :initial-contents '(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
                                              #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19)))
         (n (length octets))
         (padded (* 64 (ceiling (+ n 9) 64)))
         (message (make-array padded :element-type '(unsigned-byte 8) :initial-element 0))
         (w (make-array 64 :element-type '(unsigned-byte 32))))
    (declare (type (simple-array (unsigned-byte 32) (64)) k w)
             (type (simple-array (unsigned-byte 32) (8)) h)
             (type (simple-array (unsigned-byte 8) (*)) message)
             (type fixnum n padded))
    (replace message octets)
    (setf (aref message n) #x80)
    (loop for i from 0 below 8
          do (setf (aref message (- padded 1 i)) (ldb (byte 8 (* 8 i)) (* 8 n))))
    (macrolet ((rotr (x n) `(logior (ash ,x (- ,n)) (ldb (byte 32 0) (ash ,x (- 32 ,n)))))
               (add (&rest xs) `(ldb (byte 32 0) (+ ,@xs))))
      (loop for block of-type fixnum from 0 below padded by 64
            do (dotimes (i 16)
                 (setf (aref w i) (logior (ash (aref message (+ block (* 4 i))) 24)
                                          (ash (aref message (+ block (* 4 i) 1)) 16)
                                          (ash (aref message (+ block (* 4 i) 2)) 8)
                                          (aref message (+ block (* 4 i) 3)))))
               (loop for i from 16 below 64
                     do (let ((a (aref w (- i 15))) (b (aref w (- i 2))))
                          (setf (aref w i)
                                (add (aref w (- i 16)) (logxor (rotr a 7) (rotr a 18) (ash a -3))
                                     (aref w (- i 7)) (logxor (rotr b 17) (rotr b 19) (ash b -10))))))
               (let ((a (aref h 0)) (b (aref h 1)) (c (aref h 2)) (d (aref h 3))
                     (e (aref h 4)) (f (aref h 5)) (g (aref h 6)) (hh (aref h 7)))
                 (declare (type (unsigned-byte 32) a b c d e f g hh))
                 (dotimes (i 64)
                   (let* ((t1 (add hh (logxor (rotr e 6) (rotr e 11) (rotr e 25))
                                   (logxor (logand e f) (logand (logxor e #xffffffff) g))
                                   (aref k i) (aref w i)))
                          (t2 (add (logxor (rotr a 2) (rotr a 13) (rotr a 22))
                                   (logxor (logand a b) (logand a c) (logand b c)))))
                     (setf hh g g f f e e (add d t1) d c c b b a a (add t1 t2))))
                 (setf (aref h 0) (add (aref h 0) a) (aref h 1) (add (aref h 1) b)
                       (aref h 2) (add (aref h 2) c) (aref h 3) (add (aref h 3) d)
                       (aref h 4) (add (aref h 4) e) (aref h 5) (add (aref h 5) f)
                       (aref h 6) (add (aref h 6) g) (aref h 7) (add (aref h 7) hh)))))
    (let ((digest (make-array 32 :element-type '(unsigned-byte 8))))
      (dotimes (i 8 digest)
        (dotimes (j 4)
          (setf (aref digest (+ (* 4 i) j)) (ldb (byte 8 (* 8 (- 3 j))) (aref h i))))))))

(defparameter *base58* "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  "The characters of Base58, as Bitcoin writes numbers in them, by value.")

(defparameter *bech32* "qpzry9x8gf2tvdw0s3jn54khce6mua7l"
  "The characters of Bech32 (BIP 173), by value.")

(defun base58check-address-p (address)
  "True when ADDRESS, a string of Base58, is a Bitcoin address of the first
kinds (P2PKH, starting 1; P2SH, starting 3): 25 octets once read, a version
of 0 or 5, and its last four the first four of SHA-256 twice over the rest."
  (let ((n 0) (zeros (or (position #\1 address :test-not #'char=) (length address))))
    (loop for c across address do (setf n (+ (* n 58) (position c *base58*))))
    (let ((length (- 25 zeros)))
      (when (<= (ceiling (integer-length n) 8) length)
        (let ((octets (make-array 25 :element-type '(unsigned-byte 8) :initial-element 0)))
          (dotimes (i length) (setf (aref octets (- 24 i)) (ldb (byte 8 (* 8 i)) n)))
          (and (member (aref octets 0) '(0 5))
               (not (mismatch (sha256 (sha256 (subseq octets 0 21))) octets :start2 21 :end1 4))))))))

(defun bech32-polymod (values)
  "BIP 173's checksum of VALUES, numbers below 32."
  (let ((check 1))
    (dolist (value values check)
      (let ((top (ash check -25)))
        (setf check (logxor (ash (logand check #x1ffffff) 5) value))
        (loop for g in '(#x3b6a57b2 #x26508e6d #x1ea119fa #x3d4233dd #x2a1462b3)
              for i from 0
              when (logbitp i top) do (setf check (logxor check g)))))))

(defun segwit-address-p (address)
  "True when ADDRESS (bc1..., all small or all capitals) is a Bitcoin SegWit
address: Bech32 for version 0 (BIP 173), Bech32m for versions 1 to 16 (BIP
350), its checksum right, its program 2 to 40 octets, 20 or 32 for version 0."
  (let* ((small (string-downcase address))
         (data (map 'list (lambda (c) (position c *bech32*)) (subseq small 3))))
    (when (and (or (string= address small) (string= address (string-upcase address)))
               (every #'identity data) (>= (length data) 7))
      (let ((check (bech32-polymod (append '(3 3 0 2 3) data)))
            (version (first data))
            (bits 0) (count 0) (program 0))
        ;; the program: its five-bit values made octets, nothing left over but zeros
        (dolist (value (subseq data 1 (- (length data) 6)))
          (setf bits (logior (ash bits 5) value))
          (incf count 5)
          (when (>= count 8)
            (decf count 8)
            (incf program)
            (setf bits (ldb (byte count 0) bits))))
        (and (<= version 16) (< count 5) (zerop bits)
             (<= 2 program 40)
             (if (zerop version)
                 (and (= check 1) (member program '(20 32)))
                 (= check #x2bc830a3)))))))

(defun bitcoin-end (text start end)
  "Where the Bitcoin address that begins at START of TEXT (before END) ends,
or NIL when none does: a Base58Check one of 26 to 35 characters
\(BASE58CHECK-ADDRESS-P) or a SegWit one (SEGWIT-ADDRESS-P), followed by no
letter or digit."
  (let ((stop (or (position-if-not #'alphanumericp* text :start start :end end) end)))
    (and (if (and (<= (+ start 3) stop) (string-equal "bc1" text :start2 start :end2 (+ start 3)))
             (and (<= 14 (- stop start) 90) (segwit-address-p (subseq text start stop)))
             (and (<= 26 (- stop start) 35)
                  (every (lambda (c) (find c *base58*)) (subseq text start stop))
                  (base58check-address-p (subseq text start stop))))
         stop)))

(defparameter *keccak-round-constants*
  (coerce '(#x0000000000000001 #x0000000000008082 #x800000000000808A #x8000000080008000
            #x000000000000808B #x0000000080000001 #x8000000080008081 #x8000000000008009
            #x000000000000008A #x0000000000000088 #x0000000080008009 #x000000008000000A
            #x000000008000808B #x800000000000008B #x8000000000008089 #x8000000000008003
            #x8000000000008002 #x8000000000000080 #x000000000000800A #x800000008000000A
            #x8000000080008081 #x8000000000008080 #x0000000080000001 #x8000000080008008)
          '(simple-array (unsigned-byte 64) (24)))
  "Keccak-f[1600]'s round constants (FIPS 202).")

(defparameter *keccak-rotations*
  (coerce '(0 1 62 28 27 36 44 6 55 20 3 10 43 25 39 41 45 15 21 8 18 2 61 56 14)
          '(simple-array (unsigned-byte 8) (25)))
  "How far Keccak-f[1600] turns each lane, lane X + 5Y (FIPS 202).")

(defun sha3-256 (octets)
  "The SHA3-256 digest of OCTETS (FIPS 202), 32 octets: for the few octets a
check sums, as an onion address's."
  (let ((a (make-array 25 :element-type '(unsigned-byte 64) :initial-element 0))
        (b (make-array 25 :element-type '(unsigned-byte 64) :initial-element 0))
        (c (make-array 5 :element-type '(unsigned-byte 64) :initial-element 0))
        (padded (make-array (* 136 (ceiling (1+ (length octets)) 136)) :element-type '(unsigned-byte 8) :initial-element 0)))
    (flet ((rot (x n) (if (zerop n) x (ldb (byte 64 0) (logior (ash x n) (ash x (- n 64)))))))
      (replace padded octets)
      (setf (aref padded (length octets)) #x06)
      (setf (aref padded (1- (length padded))) (logior (aref padded (1- (length padded))) #x80))
      (loop for block from 0 below (length padded) by 136
            do (dotimes (lane 17)
                 (dotimes (k 8)
                   (setf (aref a lane) (logxor (aref a lane) (ash (aref padded (+ block (* 8 lane) k)) (* 8 k))))))
               (dotimes (round 24)
                 (dotimes (x 5)
                   (setf (aref c x) (logxor (aref a x) (aref a (+ x 5)) (aref a (+ x 10)) (aref a (+ x 15)) (aref a (+ x 20)))))
                 (dotimes (x 5)
                   (let ((d (logxor (aref c (mod (+ x 4) 5)) (rot (aref c (mod (1+ x) 5)) 1))))
                     (dotimes (y 5) (setf (aref a (+ x (* 5 y))) (logxor (aref a (+ x (* 5 y))) d)))))
                 (dotimes (x 5)
                   (dotimes (y 5)
                     (setf (aref b (+ y (* 5 (mod (+ (* 2 x) (* 3 y)) 5))))
                           (rot (aref a (+ x (* 5 y))) (aref *keccak-rotations* (+ x (* 5 y)))))))
                 (dotimes (x 5)
                   (dotimes (y 5)
                     (setf (aref a (+ x (* 5 y)))
                           (logxor (aref b (+ x (* 5 y)))
                                   (logand (logxor (aref b (+ (mod (1+ x) 5) (* 5 y))) #xFFFFFFFFFFFFFFFF)
                                           (aref b (+ (mod (+ x 2) 5) (* 5 y))))))))
                 (setf (aref a 0) (logxor (aref a 0) (aref *keccak-round-constants* round))))))
    (let ((digest (make-array 32 :element-type '(unsigned-byte 8))))
      (dotimes (i 32 digest)
        (setf (aref digest i) (ldb (byte 8 (* 8 (mod i 8))) (aref a (floor i 8))))))))

(defparameter *base32* "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"
  "The characters of Base32 (RFC 4648), by value.")

(defun onion-v3-p (name)
  "True when NAME, the 56 characters before .onion, is a version 3 onion
address (Tor's rend-spec-v3): Base32 for a key of 32 octets, two octets of
check and the version 3, the check the first two octets of SHA3-256 of
\".onion checksum\", the key and the version."
  (let ((n 0))
    (loop for ch across name do (setf n (+ (* n 32) (position (char-upcase ch) *base32*))))
    (let ((octets (make-array 35 :element-type '(unsigned-byte 8))))
      (dotimes (i 35) (setf (aref octets (- 34 i)) (ldb (byte 8 (* 8 i)) n)))
      (and (= (aref octets 34) 3)
           (let ((sum (sha3-256 (concatenate '(vector (unsigned-byte 8))
                                             (map 'vector #'char-code ".onion checksum")
                                             (subseq octets 0 32) #(3)))))
             (and (= (aref sum 0) (aref octets 32)) (= (aref sum 1) (aref octets 33))))))))

(defun onion-end (text start end)
  "Where the onion address that begins at START of TEXT (before END) ends, or
NIL when none does: 56 characters of Base32, all small or all capitals, its
check right (ONION-V3-P), or 16 for a version 2 address, which has none; then
.onion, followed by no letter, digit or hyphen."
  (let ((stop (or (position-if-not (lambda (ch) (find (char-upcase ch) *base32*)) text :start start :end end) end)))
    (and (member (- stop start) '(16 56))
         (<= (+ stop 6) end)
         (string-equal ".onion" text :start2 stop :end2 (+ stop 6))
         (let ((name (subseq text start stop)))
           (and (or (string= name (string-downcase name)) (string= name (string-upcase name)))
                (or (= (length name) 16) (onion-v3-p name))))
         (let ((after (+ stop 6)))
           (and (or (>= after end) (not (or (alphanumericp* (char text after)) (char= (char text after) #\-))))
                after)))))

(defvar *window-open* nil
  "True while a pattern is tried on a window of a longer text, more of it
after the window's end: a filter reading to that end then says it may go on.")

(defvar *window-rope* nil
  "While a pattern is tried on a window of a rope, (ROPE CONTEXT OCTET BOUND):
the rope, the octets the window's text begins and ends at, and the one past
which the pattern sees nothing. A filter may read the rope before the window
and, when the window says it may go on (*WINDOW-OPEN*), after it
(TEXT-READER).")

(defvar *window-reached* nil
  "The octet a filter's match reached, read on past the window's end
(WINDOW-INDEX); NIL while none did.")

(defun rope-reader (rope low high &optional (size 65536))
  "Two functions reading ROPE's octets from LOW below HIGH SIZE at a time:
of an offset, the octet there as a character; of an offset, a limit and a
CHAR-TABLE, the first offset from there below the limit whose octet is not in
the table, or the limit."
  (let ((buffer (tuatara.text:make-octets size)) (from 0) (to 0))
    (declare (type fixnum from to) (type tuatara.text:octets buffer))
    (values
     (lambda (at)
       (declare (type fixnum at) (optimize speed))
       (unless (and (<= from at) (< at to))
         (setf from (max low (- at (floor (length buffer) 2)))
               to (min high (+ from (length buffer))))
         (tuatara.text:rope-copy-into rope from to buffer))
       (code-char (aref buffer (- at from))))
     (lambda (at limit table)
       (declare (type fixnum at limit) (type simple-bit-vector table) (optimize speed))
       (block run
         (loop
           (when (>= at limit) (return limit))
           (unless (and (<= from at) (< at to))
             (setf from at to (min high (+ at (length buffer))))
             (tuatara.text:rope-copy-into rope from to buffer))
           (let ((stop (min to limit)))
             (loop for j of-type fixnum from at below stop
                   do (let ((b (aref buffer (- j from))))
                        (unless (and (< b 128) (= 1 (sbit table b))) (return-from run j))))
             (setf at stop))))))))

(defun char-table (predicate)
  "The ASCII characters PREDICATE is true of, as a bit for each code below 128."
  (let ((table (make-array 128 :element-type 'bit :initial-element 0)))
    (dotimes (code 128 table)
      (when (funcall predicate (code-char code)) (setf (sbit table code) 1)))))

(declaim (inline in-table-p))
(defun in-table-p (table ch)
  "True when CH is one of the characters of TABLE (CHAR-TABLE)."
  (declare (type simple-bit-vector table) (type character ch))
  (let ((code (char-code ch))) (and (< code 128) (= 1 (sbit table code)))))

(sb-ext:define-load-time-global *base32-capitals* (char-table (lambda (ch) (or (char<= #\A ch #\Z) (char<= #\2 ch #\7))))
  "The characters of Base32 written in capitals (RFC 4648).")
(sb-ext:define-load-time-global *base32-small* (char-table (lambda (ch) (or (char<= #\a ch #\z) (char<= #\2 ch #\7))))
  "The characters of Base32 written small.")
(sb-ext:define-load-time-global *base64-standard* (char-table (lambda (ch) (or (alphanumericp* ch) (find ch "+/"))))
  "The characters of Base64 (RFC 4648).")
(sb-ext:define-load-time-global *base64-safe* (char-table (lambda (ch) (or (alphanumericp* ch) (find ch "-_"))))
  "The characters of Base64 for addresses and file names, - and _ for + and /.")

(defun text-reader (text end)
  "Five values for reading TEXT, which ends at END: a function of an index
giving the character there; the index the text it reads ends at; whether that
end is a window's that says it may go on (*WINDOW-OPEN*); the first index it
reads; and a function of an index and a CHAR-TABLE giving the first index from
there whose character is not in the table, or the end. On a window of a rope
(*WINDOW-ROPE*), END the window's end, the rope is read before the window,
from its start, and after it, as far as the pattern sees, when the window may
go on: an index before 0 or past END stands for the octet as far before or
past the window, each octet as a character. So a run of ASCII characters, a
block of Base64 say, is read whole however long, for WINDOW-INDEX to give
back."
  (destructuring-bind (&optional rope context octet bound) *window-rope*
    (if rope
        (let ((back nil) (on nil) (on-run nil) (open *window-open*))
          (declare (type fixnum end context octet bound))
          ;; the rope is read only where a filter reads past the window
          (flet ((before (at)
                   (funcall (the function (or back (setf back (rope-reader rope 0 context 4096)))) at))
                 (after (at)
                   (unless on (setf (values on on-run) (rope-reader rope octet bound)))
                   (funcall (the function on) at))
                 (after-run (at table)
                   (unless on (setf (values on on-run) (rope-reader rope octet bound)))
                   (funcall (the function on-run) at bound table)))
            (declare (inline before after after-run))
            (macrolet ((reading (type)
                         `(let ((text text))
                            (declare (type ,type text))
                            (values
                             (lambda (i)
                               (declare (type fixnum i) (optimize speed))
                               (cond ((minusp i) (before (+ context i)))
                                     ((< i end) (schar text i))
                                     (t (after (+ octet (- i end))))))
                             (lambda (i table)
                               (declare (type fixnum i) (type simple-bit-vector table) (optimize speed))
                               (let ((j (loop for j of-type fixnum from i below end
                                              unless (in-table-p table (schar text j)) return j
                                              finally (return (max i end)))))
                                 (declare (type fixnum j))
                                 (if (and open (>= j end))
                                     (+ end (- (the fixnum (after-run (+ octet (- j end)) table)) octet))
                                     j)))))))
              (multiple-value-bind (char-at run)
                  (if (typep text 'simple-base-string)
                      (reading simple-base-string)
                      (reading (simple-array character (*))))
                (values char-at (if open (+ end (- bound octet)) end) nil (- context) run)))))
        (values (lambda (i) (char text i)) end *window-open* 0
                (lambda (i table)
                  (or (position-if-not (lambda (ch) (in-table-p table ch)) text :start i :end end) end))))))

(defun window-index (index end)
  "INDEX, from a TEXT-READER of a text ending at END, as an index of that
text: one at END or past it read on in the rope is kept in *WINDOW-REACHED*
as its octet and given as END."
  (if (and index *window-open* *window-rope* (>= index end))
      (progn (setf *window-reached* (+ (third *window-rope*) (- index end))) end)
      index))

(defun continued-line-p (char-at first start end table group)
  "True when START, read by CHAR-AT from FIRST, begins a line that goes on a
block of RFC 4648 encoding before it: the line before is of the block's
characters (TABLE, a CHAR-TABLE) from a start a block may have, after no +,
/, -, _ or =, its width a whole number of GROUP and 16 or more, and the line
at START no longer, before END. Such a line begins no block, the block before
taking it in."
  (declare (type function char-at) (type fixnum first start end group))
  (let ((e (1- start)))
    (declare (type fixnum e))
    (and (>= e first)
         (char= (funcall char-at e) #\Newline)
         (progn (when (and (> e first) (char= (funcall char-at (1- e)) #\Return)) (decf e)) t)
         (let ((s e))
           (declare (type fixnum s))
           (loop while (and (> s first) (in-table-p table (funcall char-at (1- s)))) do (decf s))
           (let ((width (- e s)))
             (and (>= width 16) (zerop (mod width group))
                  (or (= s first) (not (find (funcall char-at (1- s)) "+/-_=")))
                  (loop for i of-type fixnum from start below (min end (+ start width 1))
                        while (in-table-p table (funcall char-at i))
                        count t into own
                        finally (return (<= own width)))))))))

(defun encoded-block-end (char-at run first start end table group padding least open)
  "Where the block of RFC 4648 encoding that begins at START of a text (before
END), read by CHAR-AT and RUN from FIRST (TEXT-READER), ends, or NIL when
none does: none begins on a line that goes on the block before it
(CONTINUED-LINE-P). Its characters are those of TABLE (CHAR-TABLE), GROUP of
them making whole octets, and the = PADDING allows after them (the counts of
= it may hold); at least LEAST of them, a letter and a digit among them,
followed by no letter, digit, +, /, -, _ or =. It may go on over lines of one
length, a whole number of groups each and 16 or more, the last line no
longer: the block is the most lines that make one. Cut by an END that is
OPEN, a window's of a longer text, it is said to reach that end, so that the
window grows and it is read whole."
  (declare (type function char-at run) (type fixnum first start end group least) (optimize speed))
  (when (continued-line-p char-at first start end table group)
    (return-from encoded-block-end nil))
  (let ((i start) (count 0) (width nil) (letter nil) (digit nil) (best nil))
    (declare (type fixnum i count) (type (or null fixnum) width))
    (flet ((ends-here (at count pad)
             ;; the block ending at AT, after COUNT characters and PAD of =, when it is one
             (and (>= count least) letter digit
                  (if (plusp pad)
                      (and (member pad padding) (zerop (mod (+ count pad) group)))
                      (member (mod count group) (cons 0 (mapcar (lambda (p) (- group p)) padding))))
                  (or (>= at end) (not (or (alphanumericp* (funcall char-at at)) (find (funcall char-at at) "+/-_="))))
                  at)))
      (loop
        (let ((line-start i) (stop (funcall run i table)))
          (declare (type fixnum stop))
          (unless (and letter digit)
            (loop for k of-type fixnum from i below stop
                  do (let ((ch (funcall char-at k)))
                       (if (char<= #\0 ch #\9) (setf digit t) (when (alphanumericp* ch) (setf letter t))))
                  until (and letter digit)))
          (setf i stop)
          (when (and open (>= i end)) (return end))
          (let ((length (- i line-start)))
            (when (and width (> length width)) (return best))
            (incf count length)
            (unless width (setf width length))
            (let ((next (cond ((and (< i end) (char= (funcall char-at i) #\Newline)) (1+ i))
                              ((and (< (1+ i) end) (char= (funcall char-at i) #\Return) (char= (funcall char-at (1+ i)) #\Newline)) (+ i 2)))))
              (when (and open next (>= next end)) (return end))
              (if (and next (= length width) (>= width 16) (zerop (mod width group))
                       (< next end) (in-table-p table (funcall char-at next)))
                  (progn (setf best (or (ends-here i count 0) best) i next))
                  (let ((pad (loop while (and (< i end) (char= (funcall char-at i) #\=)) count t do (incf i))))
                    (return (if (and open (>= i end)) end (or (ends-here i count pad) best))))))))))))

(defun base32-end (text start end)
  "Where the Base32 block that begins at START of TEXT ends (RFC 4648), all
capitals or all small, or NIL: see ENCODED-BLOCK-END."
  (multiple-value-bind (char-at reach open first run) (text-reader text end)
    (let ((table (if (lower-case-p (char text start)) *base32-small* *base32-capitals*)))
      (window-index (encoded-block-end char-at run first start reach table 8 '(1 3 4 6) 16 open) end))))

(defun base64-end (text start end)
  "Where the Base64 block that begins at START of TEXT ends (RFC 4648), of
+ and / or of - and _ (the alphabet for addresses and file names), never
both, or NIL: see ENCODED-BLOCK-END. It holds small and capital letters."
  (multiple-value-bind (char-at reach open first run) (text-reader text end)
    (let* ((safe (loop for i from start below reach
                       for ch = (funcall char-at i)
                       while (or (alphanumericp* ch) (find ch "+/-_"))
                       thereis (find ch "-_")))
           (block (encoded-block-end char-at run first start reach (if safe *base64-safe* *base64-standard*)
                                     4 '(1 2) 32 open)))
      (and block
           (loop for i from start below block thereis (upper-case-p (funcall char-at i)))
           (loop for i from start below block thereis (lower-case-p (funcall char-at i)))
           (window-index block end)))))

(defparameter *pattern-filters*
  (list* (cons "card" (lambda () (card-filter)))
         (cons "iban" (lambda () (lambda (position) (iban-end cl-ppcre::*string* position cl-ppcre::*end-pos*))))
         (cons "bitcoin" (lambda () (lambda (position) (bitcoin-end cl-ppcre::*string* position cl-ppcre::*end-pos*))))
         (cons "onion" (lambda () (lambda (position) (onion-end cl-ppcre::*string* position cl-ppcre::*end-pos*))))
         (cons "base32" (lambda () (lambda (position) (base32-end cl-ppcre::*string* position cl-ppcre::*end-pos*))))
         (cons "base64" (lambda () (lambda (position) (base64-end cl-ppcre::*string* position cl-ppcre::*end-pos*))))
         (loop for (name) in *card-numbers*
               collect (let ((name name)) (cons (format nil "card~a" name) (lambda () (card-filter name))))))
  "The filters a named pattern may hold (*FILTER-MARK*), each (NAME . MAKER),
MAKER a function of no arguments making the filter.")

(defun card-pattern (&optional network)
  "The numbers of the card NETWORK (a name of *CARD-NUMBERS*), or of any of
them, no digit just before: read by a filter (CARD-FILTER), as they are
written, together or in groups, of the network's prefixes and lengths, the
Luhn check digit right (ISO/IEC 7812)."
  (format nil "(?<![0-9])(?=[0-9])(?<~acard~@[~a~]>)" *filter-mark* network))

(defparameter *named-patterns*
  (append
   `(("ipv4" "an IPv4 address (RFC 3986)" ,(ipv4-pattern))
     ("ipv6" "an IPv6 address, :: and an IPv4 tail too (RFC 3986, RFC 4291)" ,(ipv6-pattern))
     ("ipaddress" "an IPv4 or IPv6 address" ,(format nil "(?:~a|~a)" (ipv4-pattern) (ipv6-pattern)))
     ("macaddress" "a MAC address, AC-DE-48-00-00-80, ac:de:48:00:00:80 or acde.4800.0080 (IEEE 802)"
      ,(mac-pattern))
     ("email" "an e-mail address (the HTML standard's valid e-mail address)" ,(email-pattern))
     ("url" "a web or FTP address: http, https, ftp, ftps, a closing bracket only with its opening one (RFC 3986)"
      ,(url-pattern))
     ("uuid" "a UUID, its version and variant right, or the nil or max UUID (RFC 9562)" ,(uuid-pattern))
     ("guid" "a GUID as Windows writes it, in braces or not: any 8-4-4-4-12 hex digits" ,(guid-pattern))
     ("md5" "an MD5 digest: 32 hex digits (RFC 1321)" ,(hex-run-pattern 32))
     ("sha1" "a SHA-1 digest: 40 hex digits (FIPS 180-4)" ,(hex-run-pattern 40))
     ("sha256" "a SHA-256 digest: 64 hex digits (FIPS 180-4)" ,(hex-run-pattern 64))
     ("sha512" "a SHA-512 digest: 128 hex digits (FIPS 180-4)" ,(hex-run-pattern 128))
     ("iban" "an IBAN, its country's length and its check digits right (ISO 13616)"
      ,(format nil "(?<![A-Za-z0-9])(?=[A-Za-z]{2}[0-9]{2})(?<~aiban>)" *filter-mark*))
     ("bitcoin" "a Bitcoin address, 1..., 3... or bc1..., its checksum right (BIP 173, BIP 350)"
      ,(format nil "(?<![A-Za-z0-9])(?=[13]|[Bb][Cc]1)(?<~abitcoin>)" *filter-mark*))
     ("onion" "a Tor onion address, version 3 its check right, or version 2 (Tor's rend-spec)"
      ,(format nil "(?<![A-Za-z0-9-])(?=[A-Za-z2-7]{16})(?<~aonion>)" *filter-mark*))
     ("base32" "a block of Base32, capitals or small, padded right, over lines too (RFC 4648)"
      ,(format nil "(?<![A-Za-z0-9=])(?=[A-Z2-7]{16}|[a-z2-7]{16})(?<~abase32>)" *filter-mark*))
     ("base64" "a block of Base64, + and / or - and _, padded right, over lines too (RFC 4648)"
      ,(format nil "(?<![A-Za-z0-9+/_=-])(?=[A-Za-z0-9+/_-]{16})(?<~abase64>)" *filter-mark*))
     ("creditcard" "a payment card number of any network below, its check digit right (ISO/IEC 7812)"
      ,(card-pattern)))
   (loop for (name) in *card-numbers*
         collect (list name (format nil "a ~a card number, its check digit right" name) (card-pattern name))))
  "The named patterns, [[:ipv4:]] and the others, each (NAME WORDS PATTERN):
WORDS say what it finds, PATTERN finds it, as cl-ppcre reads it. Each stands
alone in its brackets, as a group: [[:ipv4:]]+ repeats it; it holds no group
that counts, so \\1 is still the first group you write.")

(sb-ext:defglobal **own-named-patterns** (make-hash-table :test 'equal :synchronized t)
  "The named patterns of your own (DEFINE-NAMED-PATTERN), by name: (WORDS
. PATTERN), PATTERN as you wrote it.")

(defvar *named-patterns-expanding* '()
  "The names of your own patterns being put in, innermost first, so that one
that names itself, by way of others or not, is refused.")

(defun named-pattern-p (name)
  "True when NAME names a pattern, [[:NAME:]]: one of *NAMED-PATTERNS* or your own."
  (or (assoc name *named-patterns* :test #'string=)
      (nth-value 1 (gethash name **own-named-patterns**))))

(defun named-pattern (name)
  "The pattern [[:NAME:]] stands for, as cl-ppcre reads it, or NIL when NAME
names none: one of *NAMED-PATTERNS*, or your own (DEFINE-NAMED-PATTERN) read
as every pattern is (ENGINE-PATTERN)."
  (or (third (assoc name *named-patterns* :test #'string=))
      (let ((own (gethash name **own-named-patterns**)))
        (when own
          (when (member name *named-patterns-expanding* :test #'string=)
            (refuse-pattern "invalid: [[:~a:]] names itself" name))
          (let ((*named-patterns-expanding* (cons name *named-patterns-expanding*)))
            (engine-pattern (cdr own)))))))

(defun filter-tree (tree)
  "TREE, a cl-ppcre parse tree, each mark of a filter (*FILTER-MARK*) made
the filter it names (*PATTERN-FILTERS*)."
  (cond ((and (consp tree) (eq (first tree) :named-register) (stringp (second tree))
              (uiop:string-prefix-p *filter-mark* (second tree)))
         (let ((maker (cdr (assoc (subseq (second tree) (length *filter-mark*)) *pattern-filters* :test #'string=))))
           (list :filter (funcall maker))))
        ((consp tree) (mapcar #'filter-tree tree))
        (t tree)))
