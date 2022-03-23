package com.molkky.molkky.repository;

import com.molkky.molkky.domain.Knockout;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

public interface KnockoutRepository extends JpaRepository<Knockout, String>, JpaSpecificationExecutor<Knockout> {
    Knockout findById(Integer id);
}
