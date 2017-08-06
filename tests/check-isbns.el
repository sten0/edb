;;; check-isbns.el --- part of EDB, the Emacs database

;; Copyright (C) 2004-2017 Thien-Thi Nguyen

;; Author: Bertrand Petit <elrond@imladris.frmug.fr.net>
;;	Michael Ernst <mernst@theory.lcs.mit.edu>

;; This file is part of EDB.
;;
;; EDB is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; EDB is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EDB.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Test cases for the ISBN type for EDB.
;; To run the tests, load this file, then do M-x check-isbns RET.

;;; Code:

(require 'db-isbn)
(eval-when-compile (require 'cl))

(defun check-isbns ()
  (interactive)
  (dolist (isbn succeeding-isbns)
    (db-isbn-constraint isbn nil nil nil))
  (dolist (isbn failing-isbns)
    (let ((failed nil))
      (condition-case nil
          (db-isbn-constraint isbn nil nil nil)
        (error (setq failed t)))
      (unless failed
        (error "isbn-constraint didn't fail on %s" isbn))))
  (message "ISBN check successful."))

(defvar succeeding-isbns
  '("0-14-012950-2"
    "0-330-24024-2"
    "0-345-34787-0"
    "0-345-37077-5"
    "0-380-50773-0"
    "0-441-27233-9"
    "0-553-28368-5"
    "0-553-29817-8"
    "0-575-04689-9"
    "0-586-20764-3"
    "0-586-21292-2"
    "0-812-52458-6"
    "0-8600-7901-5"
    "2-03-037003-7"
    "2-07-036822-X"
    "2-08-070320-X"
    "2-08-070330-7"
    "2-13-037888-9"
    "2-2011-0152-3"
    "2-207-22942-4"
    "2-207-22958-0"
    "2-207-30002-1"
    "2-207-30008-0"
    "2-207-30009-9"
    "2-207-30017-X"
    "2-207-30021-8"
    "2-207-30025-0"
    "2-207-30026-9"
    "2-207-30029-3"
    "2-207-30036-6"
    "2-207-30037-4"
    "2-207-30038-2"
    "2-207-30043-9"
    "2-207-30051-X"
    "2-207-30065-X"
    "2-207-30083-8"
    "2-207-30087-0"
    "2-207-30110-9"
    "2-207-30120-6"
    "2-207-30134-6"
    "2-207-30142-7"
    "2-207-30142-7"
    "2-207-30143-5"
    "2-207-30165-6"
    "2-207-30166-4"
    "2-207-30175-3"
    "2-207-30191-5"
    "2-207-30268-7"
    "2-207-30276-8"
    "2-207-30314-4"
    "2-207-30333-0"
    "2-207-30334-9"
    "2-207-30336-5"
    "2-207-30338-1"
    "2-207-30339-X"
    "2-207-30344-6"
    "2-207-30345-4"
    "2-207-30346-2"
    "2-207-30347-0"
    "2-207-30348-9"
    "2-207-30349-7"
    "2-207-30350-0"
    "2-207-30351-9"
    "2-207-30352-7"
    "2-207-30353-5"
    "2-207-30354-3"
    "2-207-30356-X"
    "2-207-30358-6"
    "2-207-30361-6"
    "2-207-30362-4"
    "2-207-30363-2"
    "2-207-30365-9"
    "2-207-30366-7"
    "2-207-30368-3"
    "2-207-30369-1"
    "2-207-30369-1"
    "2-207-30370-5"
    "2-207-30372-1"
    "2-207-30373-X"
    "2-207-30375-6"
    "2-207-30376-4"
    "2-207-30377-2"
    "2-207-30378-0"
    "2-207-30379-9"
    "2-207-30381-0"
    "2-207-30383-7"
    "2-207-30385-3"
    "2-207-30386-1"
    "2-207-30387-X"
    "2-207-30387-X"
    "2-207-30388-8"
    "2-207-30389-6"
    "2-207-30390-X"
    "2-207-30391-8"
    "2-207-30392-6"
    "2-207-30393-4"
    "2-207-30394-2"
    "2-207-30396-9"
    "2-207-30397-7"
    "2-207-30398-5"
    "2-207-30400-0"
    "2-207-30401-9"
    "2-207-30402-7"
    "2-207-30403-5"
    "2-207-30405-1"
    "2-207-30406-X"
    "2-207-30408-6"
    "2-207-30411-6"
    "2-207-30412-4"
    "2-207-30416-7"
    "2-207-30417-5"
    "2-207-30418-3"
    "2-207-30419-1"
    "2-207-30420-5"
    "2-207-30421-3"
    "2-207-30423-X"
    "2-207-30424-8"
    "2-207-30425-6"
    "2-207-30426-4"
    "2-207-30427-2"
    "2-207-30428-0"
    "2-207-30429-9"
    "2-207-30430-2"
    "2-207-30431-0"
    "2-207-30432-9"
    "2-207-30433-7"
    "2-207-30434-5"
    "2-207-30435-3"
    "2-207-30436-1"
    "2-207-30437-X"
    "2-207-30438-8"
    "2-207-30439-6"
    "2-207-30440-X"
    "2-207-30441-8"
    "2-207-30442-6"
    "2-207-30444-2"
    "2-207-30445-0"
    "2-207-30446-9"
    "2-207-30447-7"
    "2-207-30448-5"
    "2-207-30449-3"
    "2-207-30450-7"
    "2-207-30451-5"
    "2-207-30452-3"
    "2-207-30453-1"
    "2-207-30454-X"
    "2-207-30455-8"
    "2-207-30456-6"
    "2-207-30456-6"
    "2-207-30458-2"
    "2-207-30459-0"
    "2-207-30460-4"
    "2-207-30474-4"
    "2-207-30564-3"
    "2-207-30565-1"
    "2-207-34001-5"
    "2-207-34001-5"
    "2-207-34002-3"
    "2-207-34002-3"
    "2-207-34003-1"
    "2-207-34004-X"
    "2-207-34004-X"
    "2-207-34005-8"
    "2-207-34005-8"
    "2-207-34006-6"
    "2-207-34006-6"
    "2-207-34007-4"
    "2-207-34007-4"
    "2-207-34009-0"
    "2-207-34009-0"
    "2-207-34010-4"
    "2-207-34011-2"
    "2-207-34011-2"
    "2-207-34012-0"
    "2-207-34012-0"
    "2-207-34014-7"
    "2-207-50015-2"
    "2-207-50057-8"
    "2-207-50090-X"
    "2-207-50265-1"
    "2-207-50266-X"
    "2-207-50330-5"
    "2-207-50409-3"
    "2-207-50545-6"
    "2-226-00362-2"
    "2-226-00364-9"
    "2-226-00406-8"
    "2-226-00464-5"
    "2-226-00465-3"
    "2-226-00467-X"
    "2-226-00573-0"
    "2-226-00574-9"
    "2-226-00628-1"
    "2-226-00629-X"
    "2-226-00630-3"
    "2-226-00631-1"
    "2-226-00721-0"
    "2-226-00722-9"
    "2-226-00732-6"
    "2-226-00818-7"
    "2-226-00819-5"
    "2-226-00820-9"
    "2-226-01142-0"
    "2-226-01198-6"
    "2-226-01562-0"
    "2-226-01692-9"
    "2-226-01772-0"
    "2-253-00060-4"
    "2-253-00608-4"
    "2-253-00609-2"
    "2-253-00723-4"
    "2-253-01378-1"
    "2-253-01554-7"
    "2-253-01555-5"
    "2-253-01556-3"
    "2-253-01572-5"
    "2-253-01866-X"
    "2-253-02039-7"
    "2-253-02142-3"
    "2-253-02208-X"
    "2-253-02209-8"
    "2-253-02307-8"
    "2-253-02309-4"
    "2-253-02310-8"
    "2-253-02310-8"
    "2-253-02578-X"
    "2-253-02580-1"
    "2-253-02626-3"
    "2-253-03150-X"
    "2-253-03151-8"
    "2-253-03152-6"
    "2-253-03341-3"
    "2-253-03411-8"
    "2-253-03428-2"
    "2-253-03463-0"
    "2-253-03558-0"
    "2-253-03559-9"
    "2-253-03582-3"
    "2-253-03628-5"
    "2-253-03629-3"
    "2-253-03659-5"
    "2-253-03676-5"
    "2-253-03717-6"
    "2-253-03718-4"
    "2-253-03751-6"
    "2-253-03970-5"
    "2-253-04095-9"
    "2-253-04120-3"
    "2-253-04142-4"
    "2-253-04206-4"
    "2-253-04290-0"
    "2-253-04311-7"
    "2-253-04332-X"
    "2-253-04526-8"
    "2-253-04680-9"
    "2-253-04735-X"
    "2-253-04908-5"
    "2-253-05087-3"
    "2-253-05270-1"
    "2-253-05342-2"
    "2-253-05438-0"
    "2-253-05767-3"
    "2-258-01662-2"
    "2-259-00042-8"
    "2-259-00043-6"
    "2-259-00104-1"
    "2-259-00105-X"
    "2-259-00168-8"
    "2-259-00205-6"
    "2-259-00247-1"
    "2-259-00288-9"
    "2-259-00312-5"
    "2-259-00338-9"
    "2-259-00376-1"
    "2-259-00393-1"
    "2-259-00424-5"
    "2-259-00448-2"
    "2-259-00510-1"
    "2-259-00511-X"
    "2-259-00513-6"
    "2-259-00532-2"
    "2-259-00566-7"
    "2-259-00581-0"
    "2-259-00603-5"
    "2-259-00616-7"
    "2-259-00627-2"
    "2-259-00639-6"
    "2-259-00685-X"
    "2-259-00695-7"
    "2-259-00732-5"
    "2-259-00778-3"
    "2-259-00809-7"
    "2-259-00859-3"
    "2-259-00874-7"
    "2-259-00877-1"
    "2-259-00895-X"
    "2-259-00905-0"
    "2-259-00926-3"
    "2-259-00935-2"
    "2-259-00953-0"
    "2-259-00958-1"
    "2-259-00962-X"
    "2-259-00974-3"
    "2-259-00988-3"
    "2-259-00998-0"
    "2-259-01006-7"
    "2-259-01042-3"
    "2-259-01049-0"
    "2-259-01066-0"
    "2-259-01080-6"
    "2-259-01082-2"
    "2-259-01091-1"
    "2-259-01095-4"
    "2-259-01102-0"
    "2-259-01144-6"
    "2-259-01165-9"
    "2-259-01179-9"
    "2-259-01185-3"
    "2-259-01196-9"
    "2-259-01205-1"
    "2-259-01223-X"
    "2-259-01239-6"
    "2-259-01251-5"
    "2-259-01298-1"
    "2-259-01304-X"
    "2-259-01335-X"
    "2-259-01341-4"
    "2-259-01346-5"
    "2-259-01351-1"
    "2-259-01378-3"
    "2-259-01386-4"
    "2-259-01390-2"
    "2-259-01397-X"
    "2-259-01459-3"
    "2-259-01469-0"
    "2-259-01472-0"
    "2-259-01491-7"
    "2-259-01492-5"
    "2-259-01501-8"
    "2-259-01517-4"
    "2-259-01556-5"
    "2-259-01560-3"
    "2-259-01561-1"
    "2-259-01572-7"
    "2-259-01586-7"
    "2-259-01590-5"
    "2-259-01596-4"
    "2-259-01597-2"
    "2-259-01597-2"
    "2-259-01600-6"
    "2-259-01626-X"
    "2-259-01627-8"
    "2-259-01667-7"
    "2-259-01676-6"
    "2-259-01680-4"
    "2-259-01808-4"
    "2-259-01813-0"
    "2-259-01816-5"
    "2-264-00441-X"
    "2-264-00912-8"
    "2-264-00913-6"
    "2-265-01113-4"
    "2-265-01164-9"
    "2-265-03701-X"
    "2-266-00261-9"
    "2-266-00288-0"
    "2-266-00289-9"
    "2-266-00327-5"
    "2-266-00328-3"
    "2-266-00335-6"
    "2-266-00348-8"
    "2-266-00350-X"
    "2-266-00384-4"
    "2-266-00425-5"
    "2-266-00431-X"
    "2-266-00443-3"
    "2-266-00444-1"
    "2-266-00455-7"
    "2-266-00456-5"
    "2-266-00465-4"
    "2-266-00490-5"
    "2-266-00495-6"
    "2-266-00496-4"
    "2-266-00505-7"
    "2-266-00515-4"
    "2-266-00539-1"
    "2-266-00540-5"
    "2-266-00565-0"
    "2-266-00571-5"
    "2-266-00591-X"
    "2-266-00597-9"
    "2-266-00606-1"
    "2-266-00628-2"
    "2-266-00643-6"
    "2-266-00646-0"
    "2-266-00667-3"
    "2-266-00716-5"
    "2-266-00756-4"
    "2-266-00761-0"
    "2-266-00780-7"
    "2-266-00793-9"
    "2-266-00794-7"
    "2-266-00794-7"
    "2-266-00817-X"
    "2-266-00818-8"
    "2-266-00822-6"
    "2-266-00843-9"
    "2-266-00843-9"
    "2-266-00844-7"
    "2-266-00845-5"
    "2-266-00878-1"
    "2-266-00879-X"
    "2-266-00886-2"
    "2-266-00887-0"
    "2-266-00888-9"
    "2-266-00896-X"
    "2-266-00904-4"
    "2-266-00906-0"
    "2-266-00912-5"
    "2-266-00923-0"
    "2-266-00924-9"
    "2-266-00933-8"
    "2-266-00935-4"
    "2-266-00945-1"
    "2-266-00955-9"
    "2-266-00957-5"
    "2-266-00983-4"
    "2-266-00995-8"
    "2-266-00995-8"
    "2-266-01001-8"
    "2-266-01027-1"
    "2-266-01046-8"
    "2-266-01050-6"
    "2-266-01051-4"
    "2-266-01056-5"
    "2-266-01072-7"
    "2-266-01072-7"
    "2-266-01073-5"
    "2-266-01073-5"
    "2-266-01079-4"
    "2-266-01089-1"
    "2-266-01091-3"
    "2-266-01093-X"
    "2-266-01095-6"
    "2-266-01106-5"
    "2-266-01107-3"
    "2-266-01112-X"
    "2-266-01124-3"
    "2-266-01129-4"
    "2-266-01130-8"
    "2-266-01130-8"
    "2-266-01138-3"
    "2-266-01150-2"
    "2-266-01151-0"
    "2-266-01159-6"
    "2-266-01174-X"
    "2-266-01174-X"
    "2-266-01176-6"
    "2-266-01177-4"
    "2-266-01185-5"
    "2-266-01186-3"
    "2-266-01194-4"
    "2-266-01202-9"
    "2-266-01202-9"
    "2-266-01210-X"
    "2-266-01210-X"
    "2-266-01217-7"
    "2-266-01221-5"
    "2-266-01226-6"
    "2-266-01229-0"
    "2-266-01235-5"
    "2-266-01239-8"
    "2-266-01254-1"
    "2-266-01261-4"
    "2-266-01268-1"
    "2-266-01269-X"
    "2-266-01276-2"
    "2-266-01277-0"
    "2-266-01278-9"
    "2-266-01292-4"
    "2-266-01293-2"
    "2-266-01303-3"
    "2-266-01304-1"
    "2-266-01305-X"
    "2-266-01314-9"
    "2-266-01315-7"
    "2-266-01341-6"
    "2-266-01346-7"
    "2-266-01354-8"
    "2-266-01354-8"
    "2-266-01359-9"
    "2-266-01360-2"
    "2-266-01365-3"
    "2-266-01392-0"
    "2-266-01400-5"
    "2-266-01403-X"
    "2-266-01408-0"
    "2-266-01417-X"
    "2-266-01423-4"
    "2-266-01434-X"
    "2-266-01435-8"
    "2-266-01445-5"
    "2-266-01457-9"
    "2-266-01479-X"
    "2-266-01483-8"
    "2-266-01490-0"
    "2-266-01491-9"
    "2-266-01491-9"
    "2-266-01499-4"
    "2-266-01506-0"
    "2-266-01510-9"
    "2-266-01520-6"
    "2-266-01530-3"
    "2-266-01582-6"
    "2-266-01582-6"
    "2-266-01594-X"
    "2-266-01595-8"
    "2-266-01602-4"
    "2-266-01603-2"
    "2-266-01614-8"
    "2-266-01615-6"
    "2-266-01616-4"
    "2-266-01650-4"
    "2-266-01656-3"
    "2-266-01664-4"
    "2-266-01669-5"
    "2-266-01669-5"
    "2-266-01670-9"
    "2-266-01680-6"
    "2-266-01681-4"
    "2-266-01694-6"
    "2-266-01695-4"
    "2-266-01696-2"
    "2-266-01734-9"
    "2-266-01736-5"
    "2-266-01747-0"
    "2-266-01752-7"
    "2-266-01758-6"
    "2-266-01759-4"
    "2-266-01761-6"
    "2-266-01772-1"
    "2-266-01773-X"
    "2-266-01797-7"
    "2-266-01798-5"
    "2-266-01799-3"
    "2-266-01834-5"
    "2-266-01835-3"
    "2-266-01836-1"
    "2-266-01850-7"
    "2-266-01851-5"
    "2-266-01870-1"
    "2-266-01870-1"
    "2-266-01872-8"
    "2-266-01893-0"
    "2-266-01894-9"
    "2-266-01917-1"
    "2-266-01918-X"
    "2-266-01919-8"
    "2-266-01935-X"
    "2-266-01936-8"
    "2-266-01944-9"
    "2-266-01954-6"
    "2-266-01955-4"
    "2-266-01956-2"
    "2-266-01976-7"
    "2-266-02001-3"
    "2-266-02038-2"
    "2-266-02039-0"
    "2-266-02040-4"
    "2-266-02041-2"
    "2-266-02042-0"
    "2-266-02068-4"
    "2-266-02069-2"
    "2-266-02080-3"
    "2-266-02097-8"
    "2-266-02098-6"
    "2-266-02116-8"
    "2-266-02117-6"
    "2-266-02118-4"
    "2-266-02138-9"
    "2-266-02138-9"
    "2-266-02139-7"
    "2-266-02177-X"
    "2-266-02279-2"
    "2-266-02280-6"
    "2-266-02292-X"
    "2-266-02303-9"
    "2-266-02325-X"
    "2-266-02357-8"
    "2-266-02767-0"
    "2-266-03513-4"
    "2-266-03697-1"
    "2-266-03742-0"
    "2-266-04039-1"
    "2-266-04982-8"
    "2-277-11061-2"
    "2-277-11369-7"
    "2-277-11424-3"
    "2-277-11439-1"
    "2-277-11463-4"
    "2-277-11484-7"
    "2-277-11495-2"
    "2-277-11496-0"
    "2-277-11515-0"
    "2-277-11529-0"
    "2-277-11533-9"
    "2-277-11542-8"
    "2-277-11552-5"
    "2-277-11562-2"
    "2-277-11568-1"
    "2-277-11574-6"
    "2-277-11589-4"
    "2-277-11595-9"
    "2-277-11613-0"
    "2-277-11618-1"
    "2-277-11633-5"
    "2-277-11657-2"
    "2-277-11707-2"
    "2-277-11712-9"
    "2-277-11721-8"
    "2-277-11722-6"
    "2-277-11723-4"
    "2-277-11724-2"
    "2-277-11752-8"
    "2-277-11767-6"
    "2-277-11768-4"
    "2-277-11774-9"
    "2-277-11778-1"
    "2-277-11779-X"
    "2-277-11799-4"
    "2-277-11813-3"
    "2-277-11814-1"
    "2-277-11821-4"
    "2-277-11829-X"
    "2-277-11830-3"
    "2-277-11836-2"
    "2-277-11847-8"
    "2-277-11870-2"
    "2-277-11879-6"
    "2-277-11886-9"
    "2-277-11903-2"
    "2-277-11909-1"
    "2-277-11910-5"
    "2-277-11923-7"
    "2-277-11935-0"
    "2-277-11947-4"
    "2-277-11956-3"
    "2-277-11957-1"
    "2-277-11966-0"
    "2-277-11975-X"
    "2-277-11976-8"
    "2-277-11992-X"
    "2-277-11996-2"
    "2-277-11997-0"
    "2-277-12349-8"
    "2-277-12355-2"
    "2-277-12381-1"
    "2-277-12392-7"
    "2-277-12397-8"
    "2-277-12404-4"
    "2-277-12407-9"
    "2-277-12410-9"
    "2-277-12415-X"
    "2-277-12419-2"
    "2-277-12459-1"
    "2-277-12475-3"
    "2-277-12510-5"
    "2-277-12547-4"
    "2-277-12567-9"
    "2-277-12588-1"
    "2-277-12594-6"
    "2-277-12612-8"
    "2-277-12630-6"
    "2-277-12659-4"
    "2-277-13453-8"
    "2-277-13557-7"
    "2-277-21000-5"
    "2-277-21020-X"
    "2-277-21037-4"
    "2-277-21038-2"
    "2-277-21045-5"
    "2-277-21045-5"
    "2-277-21047-1"
    "2-277-21047-1"
    "2-277-21048-X"
    "2-277-21057-9"
    "2-277-21071-4"
    "2-277-21082-X"
    "2-277-21083-8"
    "2-277-21093-5"
    "2-277-21104-4"
    "2-277-21105-2"
    "2-277-21115-X"
    "2-277-21126-5"
    "2-277-21127-3"
    "2-277-21129-X"
    "2-277-21140-0"
    "2-277-21159-1"
    "2-277-21172-9"
    "2-277-21173-7"
    "2-277-21194-X"
    "2-277-21210-5"
    "2-277-21220-2"
    "2-277-21221-0"
    "2-277-21222-9"
    "2-277-21232-6"
    "2-277-21233-4"
    "2-277-21234-2"
    "2-277-21245-8"
    "2-277-21268-7"
    "2-277-21269-5"
    "2-277-21292-X"
    "2-277-21328-4"
    "2-277-21329-2"
    "2-277-21340-3"
    "2-277-21341-1"
    "2-277-21354-3"
    "2-277-21367-5"
    "2-277-21368-3"
    "2-277-21378-0"
    "2-277-21379-9"
    "2-277-21380-2"
    "2-277-21392-6"
    "2-277-21393-4"
    "2-277-21396-9"
    "2-277-21408-6"
    "2-277-21409-4"
    "2-277-21419-1"
    "2-277-21420-5"
    "2-277-21421-3"
    "2-277-21434-5"
    "2-277-21435-3"
    "2-277-21447-7"
    "2-277-21448-5"
    "2-277-21449-3"
    "2-277-21463-9"
    "2-277-21474-4"
    "2-277-21475-2"
    "2-277-21476-0"
    "2-277-21491-4"
    "2-277-21492-2"
    "2-277-21516-3"
    "2-277-21517-1"
    "2-277-21531-7"
    "2-277-21532-5"
    "2-277-21535-X"
    "2-277-21536-8"
    "2-277-21548-1"
    "2-277-21549-X"
    "2-277-21563-5"
    "2-277-21575-9"
    "2-277-21576-7"
    "2-277-21589-9"
    "2-277-21601-1"
    "2-277-21603-8"
    "2-277-21634-8"
    "2-277-21635-6"
    "2-277-21648-8"
    "2-277-21650-X"
    "2-277-21665-8"
    "2-277-21666-6"
    "2-277-21677-1"
    "2-277-21689-5"
    "2-277-21690-9"
    "2-277-21691-7"
    "2-277-21702-6"
    "2-277-21708-5"
    "2-277-21720-4"
    "2-277-21721-2"
    "2-277-21722-0"
    "2-277-21738-7"
    "2-277-21739-5"
    "2-277-21741-7"
    "2-277-21754-9"
    "2-277-21755-7"
    "2-277-21756-5"
    "2-277-21768-9"
    "2-277-21769-7"
    "2-277-21781-6"
    "2-277-21782-4"
    "2-277-21799-9"
    "2-277-21800-6"
    "2-277-21813-8"
    "2-277-21814-6"
    "2-277-21825-1"
    "2-277-21826-X"
    "2-277-21832-4"
    "2-277-21849-9"
    "2-277-21850-2"
    "2-277-21863-4"
    "2-277-21864-2"
    "2-277-21878-2"
    "2-277-21890-1"
    "2-277-21891-X"
    "2-277-21905-3"
    "2-277-21906-1"
    "2-277-21921-5"
    "2-277-21935-5"
    "2-277-21950-9"
    "2-277-21962-2"
    "2-277-21964-9"
    "2-277-21966-5"
    "2-277-21968-1"
    "2-277-21980-0"
    "2-277-21981-9"
    "2-277-21996-7"
    "2-277-21998-3"
    "2-277-22011-6"
    "2-277-22012-4"
    "2-277-22035-3"
    "2-277-22054-X"
    "2-277-22055-8"
    "2-277-22057-4"
    "2-277-22074-4"
    "2-277-22075-2"
    "2-277-22087-6"
    "2-277-22104-X"
    "2-277-22105-8"
    "2-277-22119-8"
    "2-277-22135-X"
    "2-277-22135-X"
    "2-277-22136-8"
    "2-277-22136-8"
    "2-277-22150-3"
    "2-277-22151-1"
    "2-277-22164-3"
    "2-277-22165-1"
    "2-277-22193-7"
    "2-277-22194-5"
    "2-277-22223-2"
    "2-277-22224-0"
    "2-277-22224-0"
    "2-277-22232-1"
    "2-277-22247-X"
    "2-277-22248-8"
    "2-277-22261-5"
    "2-277-22262-3"
    "2-277-22277-1"
    "2-277-22278-X"
    "2-277-22289-5"
    "2-277-22292-5"
    "2-277-22294-1"
    "2-277-22294-1"
    "2-277-22306-9"
    "2-277-22307-7"
    "2-277-22324-7"
    "2-277-22325-5"
    "2-277-22340-9"
    "2-277-22343-3"
    "2-277-22354-9"
    "2-277-22355-7"
    "2-277-22356-5"
    "2-277-22356-5"
    "2-277-22371-9"
    "2-277-22372-7"
    "2-277-22373-5"
    "2-277-22389-1"
    "2-277-22406-5"
    "2-277-22451-0"
    "2-277-22552-5"
    "2-277-22553-3"
    "2-277-22632-7"
    "2-277-22670-X"
    "2-277-22693-9"
    "2-277-22705-6"
    "2-277-22780-3"
    "2-277-23059-6"
    "2-277-51212-5"
    "2-501-00072-2"
    "2-7007-0122-4"
    "2-7024-0471-5"
    "2-7024-0695-5"
    "2-7304-0010-9"
    "2-7304-0050-8"
    "2-7304-0074-5"
    "2-7304-0155-5"
    "2-7304-0169-5"
    "2-7304-0181-4"
    "2-7304-0215-2"
    "2-7316-0182-5"
    "2-7340-0110-1"
    "2-7340-0111-X"
    "2-7340-0113-6"
    "2-7340-0115-2"
    "2-7340-0122-5"
    "2-7340-0179-9"
    "2-8011-0155-9"
    "2-8011-0156-7"
    "2-8011-0174-5"
    "2-8011-0181-8"
    "2-8011-0213-X"
    "2-8011-0238-5"
    "2-8011-0239-3"
    "2-8011-0258-X"
    "2-8011-0260-1"
    "2-8011-0290-3"
    "2-8011-0291-1"
    "2-80400-230-6"
    "2-86215-107-6"
    "2-86215-128-9"
    "2-86215-139-4"
    "2-906843-26-1"))

(defvar failing-isbns
  '("0-259-00650-7"
    "2+277-21920-7"
    "2-01-038171-4"
    "2-206-00608-8"
    "2-207-300071-4"
    "2-207-30022-5"
    "2-207-30355-7"
    "2-207-31003-1"
    "2-207-31010-4"
    "2-207-31371-3"
    "2-208-070383-8"
    "2-227-21707-7"
    "2-227-22248-8"
    "2-244-21564-3"
    "2-253-02300-X"
    "2-253-031496"
    "2-253-03170-0"
    "2-258-01715-1"
    "2-259-00696-7"
    "2-259-02169-3"
    "2-266-00942-8"
    "2-266-009885-0"
    "2-266-01426-1"
    "2-266-0245-5"
    "2-266-0365-8"
    "2-27304-0145-8"
    "2-27340-0113-6"
    "2-27340-0115-2"
    "2-277-01179-X"
    "2-277-11323-0"
    "2-277-11373-0"
    "2-277-1140-5"
    "2-277-11418-4"
    "2-277-11468-0"
    "2-277-13362-X"
    "2-277-21602-8"
    "2-277-21963"
    "2-277-2306-9"
    "2-277-37010-4"
    "2-59-00766-X"
    "2-7304-01881-"
    "2-7304-0248-1"
    "2-7304-0257-2"
    "2-7304-0360-2"
    "2-7304-0381-5"
    "2-7340-0135-Z"
    "2-86-676-278-9"
    "20-277-01917-1"))

;; check-isbns.el ends here
