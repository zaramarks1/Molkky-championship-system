package com.molkky.molkky.repository;

import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface MatchRepository extends JpaRepository<Match, String>, JpaSpecificationExecutor<Match> {
    Match findById(Integer id);

    List<Match> findMatchesByTeamsAndFinished(Team team,Boolean finished);
    List<Match> findMatchesByTeams(Team team);


    @Query(value="SELECT DISTINCT(m) FROM Match m JOIN m.round.tournament t WHERE t=:tournament ")
    List<Match> findMatchAttributedToStaff(@Param("tournament")Tournament tournament);

    @Query(value="SELECT DISTINCT(m) FROM Match m JOIN m.round.tournament t WHERE t=:tournament AND m.finished=:finished")
    List<Match> findMatchAttributedToStaffAndFinished(@Param("tournament")Tournament tournament, @Param("finished")Boolean finished);
}
