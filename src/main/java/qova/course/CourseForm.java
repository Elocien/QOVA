package qova.course;

import javax.validation.constraints.NotNull;

public class CourseForm {

    @NotNull
    private String name;

    @NotNull
    private CourseType type;

    //@NotNull
    //private ___ qrcode

    private String optQ1;
    private String optQ2;
    private String optQ3;

    public CourseForm(String name, CourseType type, String optQ1, String optQ2, String optQ3){
        this.name = name;
        this.type = type;
        this.optQ1 = optQ1;
        this.optQ2 = optQ2;
        this.optQ3 = optQ3;
        //this.qrcode = qrcode
    }

    public String getName(){
        return this.name;
    }

    public CourseType getType(){
        return this.type;
    }

    public String getOptQ1(){
        return this.optQ1;
    }

    public String getOptQ2(){
        return this.optQ2;
    }

    public String getOptQ3(){
        return this.optQ3;
    }

    



    
}