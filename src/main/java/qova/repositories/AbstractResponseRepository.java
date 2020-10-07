package qova.repositories;

import org.springframework.stereotype.Repository;

import qova.objects.AbstractResponse;

import java.util.Optional;

import org.springframework.data.repository.CrudRepository;

@Repository
public interface AbstractResponseRepository extends CrudRepository<AbstractResponse, Long> {

}
