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

(include :system ("stdio.h"))

(defun (main :int) ()
  (declare (day :int))
  (@printf (str "Enter a number (1-7) to represent a day of the week: "))
  (@scanf (str "%d") (& day))
  (case day
    (1 (@printf (str "Monday\\n")))
    (2 (@printf (str "Tuesday\\n")))
    (3 (@printf (str "Wednesday\\n")))
    (4 (@printf (str "Thursday\\n")))
    (5 (@printf (str "Friday\\n")))
    (6 (@printf (str "Saturday\\n")))
    (7 (@printf (str "Sunday\\n")))
    (t (@printf (str "Invalid day number. Please enter a number between 1 and 7.\\n"))))
  (return 0))
