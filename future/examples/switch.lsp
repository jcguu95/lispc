;; #include <stdio.h>
;; int main() {
;;     int day;
;;     printf("Enter a number (1-7) to represent a day of the week: ");
;;     scanf("%d", &day);
;;     switch (day) {
;;         case 1:
;;             printf("Monday\n");
;;             break;
;;         case 2:
;;             printf("Tuesday\n");
;;             break;
;;         case 3:
;;             printf("Wednesday\n");
;;             break;
;;         case 4:
;;             printf("Thursday\n");
;;             break;
;;         case 5:
;;             printf("Friday\n");
;;             break;
;;         case 6:
;;             printf("Saturday\n");
;;             break;
;;         case 7:
;;             printf("Sunday\n");
;;             break;
;;         default:
;;             printf("Invalid day number. Please enter a number between 1 and 7.\n");
;;             break;
;;     }
;;     return 0;
;; }

(include "stdio.h")

(defun (main :int) ()
  (def (day :int))
  (@printf (str "Enter a number (1-7) to represent a day of the week: "))
  (@scanf (str "%d") (& day))
  (case day
    (1 (@printf "Monday\\n")    (break))
    (2 (@printf "Tuesday\\n")   (break))
    (3 (@printf "Wednesday\\n") (break))
    (4 (@printf "Thursday\\n")  (break))
    (5 (@printf "Friday\\n")    (break))
    (6 (@printf "Saturday\\n")  (break))
    (7 (@printf "Sunday\\n")    (break))
    (t (@printf "Invalid day number. Please enter a number between 1 and 7.\\n") (break)))
  (return 0))
