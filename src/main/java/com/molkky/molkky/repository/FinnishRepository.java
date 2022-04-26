package com.molkky.molkky.repository;

import com.molkky.molkky.domain.rounds.Finnish;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

public interface FinnishRepository extends JpaRepository<Finnish, String>, JpaSpecificationExecutor<Finnish> {
    Finnish findById(Integer id);
}
