package com.molkky.molkky.repository;

import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.domain.UserTounamentRole;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface UserTounamentRoleRepository extends UserTournamentRoleCustom, JpaRepository<UserTounamentRole, String>, JpaSpecificationExecutor<UserTounamentRole> {

    UserTounamentRole findByTeamAndTournament(Team team, Tournament tournament);
    UserTounamentRole findByUserAndTeam(User user, Team team);
    List<UserTounamentRole> findByUser(User user);


    UserTounamentRole findById(Integer id);

    List<UserTounamentRole> findUserTounamentRoleByTournamentAndUser(Tournament tournament, User user);

}
