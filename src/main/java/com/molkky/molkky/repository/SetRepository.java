package com.molkky.molkky.repository;

import com.molkky.molkky.domain.Set;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

public interface SetRepository extends JpaRepository<Set, String>, JpaSpecificationExecutor<Set> {
    Set findById(Integer id);
}
