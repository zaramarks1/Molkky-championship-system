package com.molkky.molkky.repository;

import com.molkky.molkky.domain.SwissPool;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

public interface SwissPoolRepository extends JpaRepository<SwissPool, String>, JpaSpecificationExecutor<SwissPool> {
    SwissPool findById(Integer id);
}
