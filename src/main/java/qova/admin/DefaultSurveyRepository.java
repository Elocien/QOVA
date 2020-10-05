package qova.admin;

import org.springframework.stereotype.Repository;

import qova.enums.CourseType;

import java.util.Optional;

import org.springframework.data.repository.CrudRepository;

@Repository
public interface DefaultSurveyRepository extends CrudRepository<DefaultSurvey, Long> {

    public static final Long lectureDefaultSurveyId = 1L;
    public static final Long tutorialDefaultSurveyId = 2L;
    public static final Long seminarDefaultSurveyId = 3L;
    public static final Long practicalDefaultSurveyId = 4L;

    default DefaultSurvey findLectureSurvey() {
        Optional<DefaultSurvey> lecture = findById(1L);
        if (lecture.isEmpty()) {
            DefaultSurvey lectureDefaultSurvey = new DefaultSurvey(1L, CourseType.LECTURE);
            save(lectureDefaultSurvey);
            return lectureDefaultSurvey;
        } else {
            return lecture.get();
        }

    }

    default DefaultSurvey findTutorialSurvey() {
        Optional<DefaultSurvey> tutorial = findById(2L);
        if (tutorial.isEmpty()) {
            DefaultSurvey tutorialDefaultSurvey = new DefaultSurvey(2L, CourseType.TUTORIAL);
            save(tutorialDefaultSurvey);
            return tutorialDefaultSurvey;
        } else {
            return tutorial.get();
        }

    }

    default DefaultSurvey findSeminarSurvey() {
        Optional<DefaultSurvey> seminar = findById(3L);
        if (seminar.isEmpty()) {
            DefaultSurvey seminarDefaultSurvey = new DefaultSurvey(3L, CourseType.SEMINAR);
            save(seminarDefaultSurvey);
            return seminarDefaultSurvey;
        } else {
            return seminar.get();
        }

    }

    default DefaultSurvey findPracticalSurvey() {
        Optional<DefaultSurvey> practical = findById(4L);
        if (practical.isEmpty()) {
            DefaultSurvey practicalDefaultSurvey = new DefaultSurvey(4L, CourseType.PRACTICAL);
            save(practicalDefaultSurvey);
            return practicalDefaultSurvey;
        } else {
            return practical.get();
        }

    }

    default DefaultSurvey findDefaultSurveyForType(CourseType courseType) {
        switch (courseType) {
            case LECTURE:
                return findLectureSurvey();

            case TUTORIAL:
                return findTutorialSurvey();

            case SEMINAR:
                return findSeminarSurvey();

            case PRACTICAL:
                return findPracticalSurvey();

            default:
                return null;
        }
    }
}