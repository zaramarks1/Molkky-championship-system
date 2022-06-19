package com.molkky.molkky.repository;

import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.domain.UserTournamentRole;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import org.springframework.data.repository.query.Param;
import type.UserRole;

import java.util.List;

@Repository
public interface UserTournamentRoleRepository extends UserTournamentRoleCustom, JpaRepository<UserTournamentRole, String>, JpaSpecificationExecutor<UserTournamentRole> {

    List<UserTournamentRole> findByTeamAndTournament(Team team, Tournament tournament);
    UserTournamentRole findByUserAndTeam(User user, Team team);
    List<UserTournamentRole> findByUser(User user);
    List<UserTournamentRole> findByTeam(Team team);


    UserTournamentRole findById(Integer id);

    List<UserTournamentRole> findUserTournamentRoleByTournamentAndUser(Tournament tournament, User user);

    List<UserTournamentRole> findUserTournamentRoleByRoleAndAndTournament(UserRole role, Tournament tournament);

    @Query(value="SELECT u FROM UserTournamentRole u WHERE u.user.id=:iduser")
    UserTournamentRole findUserTournamentRoleByUserId(@Param("iduser") Integer iduser);

    List<UserTournamentRole> findUserTournamentRoleByRoleAndTournament(UserRole role, Tournament tournament);
    List<UserTournamentRole> findUserTournamentRoleByTournamentAndUserAndRole(Tournament tournament, User user, UserRole role);

}
