package com.molkky.molkky.repository;

import com.molkky.molkky.domain.Club;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

@Repository
public interface ClubRepository extends JpaRepository<Club, String>, JpaSpecificationExecutor<Club> {
    Club findById(Integer id);
    Club findByName(String name);
}