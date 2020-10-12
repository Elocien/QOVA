package qova.admin;

import org.springframework.stereotype.Repository;

import qova.enums.CourseType;

import java.util.Optional;

import org.springframework.data.repository.CrudRepository;

@Repository
public interface DefaultSurveyRepository extends CrudRepository<DefaultSurvey, Long> {

    Optional<DefaultSurvey> findByCourseType(CourseType type);

    default DefaultSurvey findLectureSurvey() {
        Optional<DefaultSurvey> lecture = findByCourseType(CourseType.LECTURE);
        if (lecture.isEmpty()) {
            DefaultSurvey lectureDefaultSurvey = new DefaultSurvey(CourseType.LECTURE);
            save(lectureDefaultSurvey);
            return lectureDefaultSurvey;
        } else {
            return lecture.get();
        }

    }

    default DefaultSurvey findTutorialSurvey() {
        Optional<DefaultSurvey> tutorial = findByCourseType(CourseType.TUTORIAL);
        if (tutorial.isEmpty()) {
            DefaultSurvey tutorialDefaultSurvey = new DefaultSurvey(CourseType.TUTORIAL);
            save(tutorialDefaultSurvey);
            return tutorialDefaultSurvey;
        } else {
            return tutorial.get();
        }

    }

    default DefaultSurvey findSeminarSurvey() {
        Optional<DefaultSurvey> seminar = findByCourseType(CourseType.SEMINAR);
        if (seminar.isEmpty()) {
            DefaultSurvey seminarDefaultSurvey = new DefaultSurvey(CourseType.SEMINAR);
            save(seminarDefaultSurvey);
            return seminarDefaultSurvey;
        } else {
            return seminar.get();
        }

    }

    default DefaultSurvey findPracticalSurvey() {
        Optional<DefaultSurvey> practical = findByCourseType(CourseType.PRACTICAL);
        if (practical.isEmpty()) {
            DefaultSurvey practicalDefaultSurvey = new DefaultSurvey(CourseType.PRACTICAL);
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