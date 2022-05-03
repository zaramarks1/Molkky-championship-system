package com.molkky.molkky.repository;

import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.domain.UserTounamentRole;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface UserTournamentRoleCustom {

    @Query(value="SELECT u FROM UserTounamentRole u LEFT JOIN u.team t WHERE u.user=:user AND t.code=:code")
    List<UserTounamentRole> findUserWithCode(@Param("user") User user, @Param("code") String code);

    @Query(value="SELECT u FROM UserTounamentRole u WHERE u.user=:user AND (u.role = 'STAFF' OR u.role = 'ADM') ")
    List<UserTounamentRole> findUserAdminStaff(@Param("user") User user);

    @Query(value="SELECT u FROM UserTounamentRole u WHERE u.user=:user AND (u.role = 'STAFF' OR u.role = 'ADM')  ")
    List<UserTounamentRole> findUserAdminStaffSameTournament(@Param("user") User user);

    @Query(value="SELECT DISTINCT u.tournament FROM UserTounamentRole u WHERE u.user=:user")
    List<Tournament> findTournamentFromUser(@Param("user") User user);


}
