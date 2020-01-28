package qova.course;

import javax.validation.constraints.NotNull;

public class courseForm{

    @NotNull
    private String name;

    @NotNull
    private courseType type;

    //@NotNull
    //private ___ qrcode

    private String optQ1;
    private String optQ2;
    private String optQ3;

    public courseForm(String name, courseType type, String optQ1, String optQ2, String optQ3){
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

    public courseType getType(){
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