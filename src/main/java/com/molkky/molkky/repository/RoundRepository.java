package com.molkky.molkky.repository;

import com.molkky.molkky.domain.Round;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;


@Repository
public interface RoundRepository extends JpaRepository<Round, String>, JpaSpecificationExecutor<Round> {
    Round findById(Integer id);
}
