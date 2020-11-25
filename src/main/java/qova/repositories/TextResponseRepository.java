package qova.repositories;

import org.springframework.stereotype.Repository;

import qova.objects.TextResponse;

import org.springframework.data.repository.CrudRepository;

import java.util.UUID;

@Repository
public interface TextResponseRepository extends CrudRepository<TextResponse, UUID> {

}