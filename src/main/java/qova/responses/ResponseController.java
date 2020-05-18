package qova.responses;

import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;

import qova.course.SurveyForm;

@Controller // This means that this class is a Controller
public class ResponseController{


    @Autowired
    private final ResponseManagement responseManagement;

    @Autowired
    private final ResponseRepository responseRepository;

    @Autowired
    ResponseController(ResponseManagement responseManagement, ResponseRepository responseRepository) {
        this.responseManagement = Objects.requireNonNull(responseManagement);
        this.responseRepository = Objects.requireNonNull(responseRepository);
    }


    
    //PostMapping to submit survey and serialize results
    //---------------------------------------------------------------------------
    @PostMapping("/survey")
    public ResponseEntity recieveResponseJSON(SurveyForm form, @RequestParam String type, @RequestParam(required = false) String id){
        
        //get JSON Response as string
        String JsonResponse = form.getQuestionnairejson();
        

        //Deserialize the String to a JavaObject Response (package qova.responses)
        // response = Deserialize(JsonResponse);

        //Save object 
        // responseRepository.save(response)
        
        //if all goes well
        return ResponseEntity.ok(HttpStatus.OK);
    }

    //---------------------------------------------------------------------------
}