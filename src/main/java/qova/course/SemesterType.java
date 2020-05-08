package qova.course;

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