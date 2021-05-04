package qova.enums;

/**
 * <p>Enumeration describing the 4 categories of course types. </p>
 * <p>A lecture is the most unique of the four, as a lecture can only have one group, as it is assumed that all participants take part in one lecture</p>
 * <p>Each of the types have individual default surveys, that are set in the {@linkplain qova.admin.DefaultSurvey} class, as JSON strings</p>
 * <p>When viewing results for a course, one can accumulate all results by instance and group, however it is not possible to accumulate all results over different
 * course types, as these have differing surveys</p>
 */
public enum CourseType {
    LECTURE{
        public String toString(){
            return "lecture";
        }
    },
    TUTORIAL{
        public String toString(){
            return "tutorial";
        }
    },
    SEMINAR{
        public String toString(){
            return "seminar";
        }
    },
    PRACTICAL{
        public String toString(){
            return "practical";
        }
    };
}