package com.molkky.molkky.repository;

import com.molkky.molkky.domain.rounds.Pool;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

public interface PoolRepository  extends JpaRepository<Pool, String>, JpaSpecificationExecutor<Pool> {
    Pool findById(Integer id);
}
