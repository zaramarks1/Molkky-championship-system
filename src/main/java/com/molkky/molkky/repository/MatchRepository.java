package com.molkky.molkky.repository;

import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface MatchRepository extends JpaRepository<Match, String>, JpaSpecificationExecutor<Match> {
    Match findById(Integer id);

    @Query(value="SELECT DISTINCT(m) FROM Match m JOIN m.round.tournament t WHERE t=:tournament")
    List<Match> findMatchAttributedToStaff(@Param("tournament")Tournament tournament);
}
