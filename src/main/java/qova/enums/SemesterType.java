package qova.enums;

/**
 * The two types of semesters in the German university system, with WiSe being "Winter Semester" and SoSe being "Summer Semester"
 */
public enum SemesterType{
    WiSe{
        public String toString(){
            return "WiSe";
        }
    },
    
    SoSe{
        public String toString(){
            return "SoSe";
        }
    };
}