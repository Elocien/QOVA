package qova.course;

import lombok.NoArgsConstructor;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;

@Entity
public class Course {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private long id;

    private String name;
    private courseType type;
    //private ___ qrcode;
    private String optQ1;
    private String optQ2;
    private String optQ3;


    //Needed for JPA purposes
    @SuppressWarnings("unused")
	protected Course() {
    }
    
    public Course(String name, courseType type, String optQ1, String optQ2, String optQ3){
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

    public void setName(String name){
        this.name = name;
    }

    public courseType getType(){
        return this.type;
    }

    public void setType(courseType type){
        this.type = type;
    }

    // public ___ getQrcode(){
    //    return this.qrcode
    //}

    //public void setQrcode(___ qrcode){
    //    this.qrcode = qrcode;
    //}

    public String getOptQ1(){
        return this.optQ1;
    }

    public void setOptQ1(String optQ1){
        this.optQ1 = optQ1;
    }

    public String getOptQ2(){
        return this.optQ2;
    }

    public void setOptQ2(String optQ2){
        this.optQ2 = optQ2;
    }

    public String getOptQ3(){
        return this.optQ3;
    }

    public void setOptQ3(String optQ3){
        this.optQ3 = optQ3;
    }
   

}