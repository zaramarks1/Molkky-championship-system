package com.molkky.molkky.model;

import com.molkky.molkky.domain.Court;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
public class CourtModel {
    private Integer id;
    private boolean isAvailable;
    private String name;

    public CourtModel(Court court) {
        this.id = court.getId();
        this.isAvailable = court.isAvailable();
        this.name = court.getName();
    }

    public static List<CourtModel> createCourtModels(List<Court> courts){
        List<CourtModel> courtModels = new ArrayList<>();
        for (Court court : courts) {
            if(court.getId() != null) {
                courtModels.add(new CourtModel(court));
            }
        }
        return courtModels;
    }
}
