package com.molkky.molkky.repository;

import com.molkky.molkky.domain.rounds.SimpleGame;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

public interface SimpleGameRepository extends JpaRepository<SimpleGame, String>, JpaSpecificationExecutor<SimpleGame> {
    SimpleGame findById(Integer id);
}
