package com.molkky.molkky.repository;

import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.domain.UserTournamentRole;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface UserTournamentRoleRepository extends UserTournamentRoleCustom, JpaRepository<UserTournamentRole, String>, JpaSpecificationExecutor<UserTournamentRole> {

    UserTournamentRole findByTeamAndTournament(Team team, Tournament tournament);
    UserTournamentRole findByUserAndTeam(User user, Team team);
    List<UserTournamentRole> findByUser(User user);


    UserTournamentRole findById(Integer id);

    List<UserTournamentRole> findUserTounamentRoleByTournamentAndUser(Tournament tournament, User user);

}
