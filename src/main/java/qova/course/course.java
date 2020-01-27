package qova.course;

public class course{
    private String name;
    private courseType type;
    // QR code goes here, not sure what file type this is 
    private String optQ1;
    private String optQ2;
    private String optQ3;


    //Needed for JPA purposes
    @SuppressWarnings("unused")
	private course() {
    }
    
    public course(String name, courseType type, String optQ1, String optQ2, String optQ3){
        
    }

}