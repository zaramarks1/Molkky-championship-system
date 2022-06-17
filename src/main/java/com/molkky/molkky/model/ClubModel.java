package com.molkky.molkky.model;

import com.molkky.molkky.domain.Club;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
public class ClubModel {
    private Integer id;
    private String name;

    public ClubModel(Club club) {
        this.id = club.getId();
        this.name = club.getName();
    }

    public static List<ClubModel> createClubModels(List<Club> clubs){
        List<ClubModel> clubModels = new ArrayList<>();
        for (Club club : clubs) {
            if(club.getId() != null) {
                clubModels.add(new ClubModel(club));
            }
        }
        return clubModels;
    }
}
