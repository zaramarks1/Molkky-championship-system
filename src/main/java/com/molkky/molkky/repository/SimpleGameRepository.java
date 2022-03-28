package com.molkky.molkky.repository;

import com.molkky.molkky.domain.Pool;
import com.molkky.molkky.domain.SimpleGame;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

public interface SimpleGameRepository extends JpaRepository<SimpleGame, String>, JpaSpecificationExecutor<SimpleGame> {
    SimpleGame findById(Integer id);
}
