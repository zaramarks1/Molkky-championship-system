package com.molkky.molkky.model;

import com.molkky.molkky.domain.UserTournamentRole;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import type.UserRole;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class UserTournamentRoleModel {
    private Integer id;
    private UserRole role;
    private Boolean isRegistered;

    public UserTournamentRoleModel(UserTournamentRole user) {
        this.id = user.getId();
        this.role = user.getRole();
        this.isRegistered = user.getIsRegistered();
    }
}
